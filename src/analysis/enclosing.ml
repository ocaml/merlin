open Std

let ( << ) (x : Location.t) (y : Location.t) =
  x.loc_end.pos_cnum <= y.loc_start.pos_cnum

let rec expand_node (nodes : Browse_raw.node list) =
  match nodes with
  | [] -> []
  | Expression { exp_desc = Texp_let (_, first_vb :: _, exp); _ } :: q ->
    let vb_loc = Mbrowse.node_loc (Value_binding first_vb) in
    let body_exp = Browse_raw.node_merlin_loc Location.none (Expression exp) in
    let loc = { vb_loc with loc_end = body_exp.loc_start } in
    loc :: expand_node (Expression exp :: q)
  | Expression { exp_desc = Texp_sequence (exp1, exp2); _ } :: q ->
    let exp1_loc = Browse_raw.node_merlin_loc Location.none (Expression exp1) in
    exp1_loc :: expand_node (Expression exp2 :: q)
  | Expression
      { exp_desc =
          Texp_match
            (exp, [ { c_lhs; c_rhs; c_guard = None; c_cont = _ } ], [], _);
        exp_loc;
        _
      }
    :: q
    when c_lhs.pat_loc << exp.exp_loc ->
    let body_exp =
      Browse_raw.node_merlin_loc Location.none (Expression c_rhs)
    in
    let let_in = { exp_loc with loc_end = body_exp.loc_start } in
    let_in :: expand_node (Expression c_rhs :: q)
  | (Value_binding _ as _vb_node)
    :: (Expression { exp_desc = Texp_let (_, first_vb :: _, exp); _ } :: _ as q)
    ->
    let vb_loc = Mbrowse.node_loc (Value_binding first_vb) in
    let body_exp = Browse_raw.node_merlin_loc Location.none (Expression exp) in
    let loc = { vb_loc with loc_end = body_exp.loc_start } in
    loc :: expand_node q
  | node :: q -> Mbrowse.node_loc node :: expand_node q

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
  (* TODO: deal with loc_ghost *)
  { loc1 with loc_start; loc_end }

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
  (* |> List.concat_map ~f:(fun (_env, node) -> *)
  (*     match node with *)
  (*     | _ -> [ node ]) *)
  |> expand_node
  |> make_expand
  |>
  (* We remove possible duplicates from the list *)
  List.fold_left ~init:[] ~f:(fun acc loc ->
      match acc with
      | hd :: _ as acc when Location_aux.compare hd loc = 0 -> acc
      | _ -> loc :: acc)
  |> List.rev
