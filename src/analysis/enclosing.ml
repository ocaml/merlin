open Std

let ( << ) (x : Location.t) (y : Location.t) =
  x.loc_end.pos_cnum <= y.loc_start.pos_cnum

let contains (loc1 : Location.t) (loc2 : Location.t) =
  if Location.is_none loc1 then false
  else
    loc1.loc_start.pos_cnum <= loc2.loc_start.pos_cnum
    && loc2.loc_end.pos_cnum <= loc1.loc_end.pos_cnum

let ( <= ) x y = contains y x

let merge (loc1 : Location.t) (loc2 : Location.t) =
  let resolve_non_pos (pos1 : Lexing.position) (pos2 : Lexing.position) f =
    if pos1.pos_cnum < 0 then pos2 else if pos2.pos_cnum < 0 then pos1 else f ()
  in
  let min_pos pos1 pos2 =
    resolve_non_pos pos1 pos2 @@ fun () ->
    if pos1.pos_cnum < pos2.pos_cnum then pos1 else pos2
  in
  let max_pos pos1 pos2 =
    resolve_non_pos pos1 pos2 @@ fun () ->
    if pos1.pos_cnum < pos2.pos_cnum then pos2 else pos1
  in
  let loc_start = min_pos loc1.loc_start loc2.loc_start in
  let loc_end = max_pos loc1.loc_end loc2.loc_end in
  { Location.loc_start; loc_end; loc_ghost = loc1.loc_ghost || loc2.loc_ghost }

let ( ++ ) = merge

(* In some cases, we don't want to go "up" to a full node, but include new
   location little by little. For instance:

   {[
   let a = 1 in
   let b = 2 in
   let c = 3 in ...
   ]}

   We don't want to go go up from [2] to [let b = 2 in let c 3 in ...], we
   want intermediate stages that do not correspond to actual nodes:
   [let b = 2 in], then [let b = 2 in let c = 3 in], ...

   Similar example can be done with chains of [;] sequences, and other
   cases. All those cases share a similar setting: they look like a sequence (of
   [let ... in ...], of [;], etc) that are actually represented with binary
   operators, with "right" priority for parentheses.

   In all those cases, we split the location in the left part and the right part
   (using [merlin_loc] to split at the right point), include the left part
   first, and "go down-right" instead of up if the current loc does not include
   the right part.
*)

let rec expand_node ~current_loc (nodes : Browse_raw.node list) =
  let may_go_down ~current_loc ~right_node node nodes =
    let right_loc = Mbrowse.node_merlin_loc right_node in
    let full_loc = Mbrowse.node_loc node in
    let left_loc = { full_loc with loc_end = right_loc.loc_start } in
    let current_loc = current_loc ++ left_loc in
    if right_loc <= current_loc then
      current_loc :: expand_node ~current_loc nodes
    else current_loc :: expand_node ~current_loc (right_node :: node :: nodes)
  in
  match nodes with
  | [] -> []
  | node :: nodes when Mbrowse.node_loc node <= current_loc ->
    expand_node ~current_loc nodes
  (* skipping "[let x = 5] in exp2" location.
     TODO: would we want to add "let [x = 5] in exp2"? *)
  | Value_binding _ :: (Expression _ :: _ as nodes) ->
    expand_node ~current_loc nodes
  (* [let x = exp1 in][ exp2] *)
  | (Expression { exp_desc = Texp_let (_, _, exp); _ } as node) :: nodes ->
    let right_node = Browse_raw.Expression exp in
    may_go_down ~current_loc ~right_node node nodes
  (* [exp1 ;][ exp2] *)
  | (Expression { exp_desc = Texp_sequence (_, exp2); _ } as node) :: nodes ->
    let right_node = Browse_raw.Expression exp2 in
    may_go_down ~current_loc ~right_node node nodes
  (* [let%map x = exp1 in][ exp2] *)
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
    :: nodes
    when pat.pat_loc << value_binded.exp_loc
         && value_binded.exp_loc << body.exp_loc ->
    let right_node = Browse_raw.Expression body in
    may_go_down ~current_loc ~right_node node nodes
  (* [let () = exp1 in][ exp2] *)
  | (Expression
       { exp_desc =
           Texp_match
             (exp, [ { c_lhs; c_rhs; c_guard = None; c_cont = _ } ], [], _);
         _
       } as node)
    :: nodes
    when c_lhs.pat_loc << exp.exp_loc ->
    let right_node = Browse_raw.Expression c_rhs in
    may_go_down ~current_loc ~right_node node nodes
  (* [let* x = exp1 in][ exp2] *)
  | (Expression { exp_desc = Texp_letop { body; _ }; _ } as node) :: nodes ->
    let right_node = Browse_raw.Expression body.c_rhs in
    may_go_down ~current_loc ~right_node node nodes
  | node :: q ->
    let loc = Mbrowse.node_loc node in
    let current_loc = current_loc ++ loc in
    current_loc :: expand_node ~current_loc q

let locs (mbrowse : Mbrowse.t) =
  mbrowse |> List.map ~f:snd
  |> expand_node ~current_loc:Location.none
  |> List.fold_left ~init:[] ~f:(fun acc loc ->
      match acc with
      | hd :: _ as acc when Location_aux.compare hd loc = 0 -> acc
      | _ -> loc :: acc)
  |> List.rev
