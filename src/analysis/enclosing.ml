open Std

let ( << ) (x : Location.t) (y : Location.t) =
  x.loc_end.pos_cnum <= y.loc_start.pos_cnum

let before (x : Location.t) (y : Location.t) =
  x.loc_start.pos_cnum < y.loc_start.pos_cnum

let is_valid_loc (loc : Location.t) =
  loc.loc_start.pos_cnum >= 0
  && loc.loc_end.pos_cnum >= 0
  && loc.loc_start.pos_cnum <= loc.loc_end.pos_cnum

let contains (loc1 : Location.t) (loc2 : Location.t) =
  loc1.loc_start.pos_cnum <= loc2.loc_start.pos_cnum
  && loc2.loc_end.pos_cnum <= loc1.loc_end.pos_cnum

let min_pos (pos1 : Lexing.position) (pos2 : Lexing.position) =
  if pos1.pos_cnum < 0 then pos2
  else if pos2.pos_cnum < 0 then pos1
  else if pos1.pos_cnum < pos2.pos_cnum then pos1
  else pos2

let max_pos (pos1 : Lexing.position) (pos2 : Lexing.position) =
  if pos1.pos_cnum < 0 then pos2
  else if pos2.pos_cnum < 0 then pos1
  else if pos1.pos_cnum < pos2.pos_cnum then pos2
  else pos1

let merge (loc1 : Location.t) (loc2 : Location.t) =
  let loc_start = min_pos loc1.loc_start loc2.loc_start in
  let loc_end = max_pos loc1.loc_end loc2.loc_end in
  { Location.loc_start; loc_end; loc_ghost = false }

let rec expand_node ~current_loc (nodes : Browse_raw.node list) =
  let get_binding_loc vb exp =
    let vb_loc = Mbrowse.node_loc (Value_binding vb) in
    let body_exp = Browse_raw.node_merlin_loc Location.none (Expression exp) in
    { vb_loc with loc_end = body_exp.loc_start }
  in
  match nodes with
  | [] -> []
  | Expression { exp_desc = Texp_let (_, first_vb :: _, exp); _ } :: q ->
    let loc = get_binding_loc first_vb exp in
    let body_loc = Browse_raw.node_merlin_loc Location.none (Expression exp) in
    if contains current_loc body_loc then
      if is_valid_loc loc then
        if contains current_loc loc then
          expand_node ~current_loc q
        else
          let new_loc = merge current_loc loc in
          loc :: expand_node ~current_loc:new_loc q
      else
        expand_node ~current_loc q
    else
      if is_valid_loc loc then
        let new_loc = merge current_loc loc in
        loc :: expand_node ~current_loc:new_loc (Expression exp :: q)
      else
        expand_node ~current_loc (Expression exp :: q)
  | Expression { exp_desc = Texp_sequence (exp1, exp2); _ } :: q ->
    let exp1_loc = Browse_raw.node_merlin_loc Location.none (Expression exp1) in
    let exp2_loc = Browse_raw.node_merlin_loc Location.none (Expression exp2) in
    if contains current_loc exp2_loc then
      expand_node ~current_loc q
    else
      if is_valid_loc exp1_loc then
        let new_loc = merge current_loc exp1_loc in
        exp1_loc :: expand_node ~current_loc:new_loc (Expression exp2 :: q)
      else
        expand_node ~current_loc (Expression exp2 :: q)
  | (Expression
       { exp_desc =
           Texp_apply
             ( _map_or_bind,
               [ (Nolabel, Arg value_binded);
                 ( Labelled "f",
                   Arg
                     { exp_desc =
                         Texp_function
                           ( [ { fp_kind = Tparam_pat pat; _ } ],
                             Tfunction_body body );
                       _
                     } )
               ] );
         _
       } as node)
    :: q
    when before pat.pat_loc value_binded.exp_loc
         && before value_binded.exp_loc body.exp_loc ->
    let body_loc = Browse_raw.node_merlin_loc Location.none (Expression body) in
    let vb_loc = Mbrowse.node_loc node in
    let loc = { vb_loc with loc_end = body_loc.loc_start } in
    if contains current_loc body_loc then
      if is_valid_loc loc then
        if contains current_loc loc then
          expand_node ~current_loc q
        else
          let new_loc = merge current_loc loc in
          loc :: expand_node ~current_loc:new_loc q
      else
        expand_node ~current_loc q
    else
      if is_valid_loc loc then
        let new_loc = merge current_loc loc in
        loc :: expand_node ~current_loc:new_loc (Expression body :: q)
      else
        expand_node ~current_loc (Expression body :: q)
  | Expression
      { exp_desc =
          Texp_match
            (exp, [ { c_lhs; c_rhs; c_guard = None; c_cont = _ } ], [], _);
        exp_loc;
        _
      }
    :: q
    when before c_lhs.pat_loc exp.exp_loc ->
    let body_loc = Browse_raw.node_merlin_loc Location.none (Expression c_rhs) in
    let let_in = { exp_loc with loc_end = body_loc.loc_start } in
    if contains current_loc body_loc then
      if is_valid_loc let_in then
        if contains current_loc let_in then
          expand_node ~current_loc q
        else
          let new_loc = merge current_loc let_in in
          let_in :: expand_node ~current_loc:new_loc q
      else
        expand_node ~current_loc q
    else
      if is_valid_loc let_in then
        let new_loc = merge current_loc let_in in
        let_in :: expand_node ~current_loc:new_loc (Expression c_rhs :: q)
      else
        expand_node ~current_loc (Expression c_rhs :: q)
  | (Value_binding _ as _vb_node)
    :: (Expression { exp_desc = Texp_let (_, first_vb :: _, exp); _ } :: _ as q)
    ->
    let loc = get_binding_loc first_vb exp in
    if is_valid_loc loc then
      if contains current_loc loc then
        expand_node ~current_loc q
      else
        let new_loc = merge current_loc loc in
        loc :: expand_node ~current_loc:new_loc q
    else
      expand_node ~current_loc q
  | node :: q ->
    let loc = Mbrowse.node_loc node in
    if is_valid_loc loc then
      if contains current_loc loc then
        expand_node ~current_loc q
      else
        let new_loc = merge current_loc loc in
        loc :: expand_node ~current_loc:new_loc q
    else
      expand_node ~current_loc q

let make_expand l =
  match l with
  | [] -> []
  | loc1 :: _ ->
    let _acc, l =
      List.fold_left_map
        ~f:(fun x y -> merge x y |> fun x -> (x, x))
        ~init:loc1 l
    in
    l

let locs (mbrowse : Mbrowse.t) =
  mbrowse |> List.map ~f:snd
  |> expand_node ~current_loc:Location.none
  |> make_expand
  |> List.fold_left ~init:[] ~f:(fun acc loc ->
         match acc with
         | hd :: _ as acc when Location_aux.compare hd loc = 0 -> acc
         | _ -> loc :: acc)
  |> List.rev
