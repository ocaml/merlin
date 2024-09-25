open Std

(** [qual_or_unqual_path mode ~open_lident ~open_path node_path node_lid] 
    if mode is 
      `Unqualify - returns [node_lid] or [node_lid] with prefix [open_lident] cut off, 
        whichever is shorter

      `Qualify - returns [node_path] with its prefix equal to [open_lident]

    Returns [None] if [node_lid] doesn't need changes.

    Note: by "prefix" we mean the leftmost consecutive part of a longident or a path. *)
let qual_or_unqual_path mode ~open_lident ~open_path node_path node_lid =
  let open_lid_head = Longident.head open_lident in
  let node_lid_head = Longident.head node_lid in
  let rec make_new_node_lid acc (p : Path.t) =
    match p with
    | Pident ident -> Ident.name ident :: acc
    | Pdot (path', s)
      when mode = `Unqualify
           && (Path.same open_path path'
              || String.equal s
                   node_lid_head (* unqualify shouldn't enlarge lident *)) ->
      s :: acc
    | Pdot (_, s) when mode = `Qualify && s = open_lid_head -> s :: acc
    | Pdot (path', s) -> make_new_node_lid (s :: acc) path'
    | _ -> raise Not_found
  in
  let same_longident node_lid_head new_node_lid =
    (* this works because [make_new_node_lid] changes only prefix of a longident *)
    String.equal node_lid_head (List.hd new_node_lid)
  in
  match make_new_node_lid [] node_path with
  | new_node_lid when not (same_longident node_lid_head new_node_lid) ->
    Some (String.concat ~sep:"." new_node_lid)
  | _ | (exception Not_found) -> None

let get_rewrites ~mode typer pos =
  match Mbrowse.select_open_node (Mtyper.node_at typer pos) with
  | None | Some (_, _, []) -> []
  | Some (open_path, open_lident, (_, node) :: _) ->
    let paths_and_lids = Browse_tree.all_occurrences_of_prefix open_path node in
    List.filter_map paths_and_lids
      ~f:(fun ({ Location.txt = path; loc }, lid) ->
        if loc.Location.loc_ghost || Location_aux.compare_pos pos loc > 0 then
          None
        else
          qual_or_unqual_path mode ~open_lident ~open_path path lid
          |> Option.map ~f:(fun new_lid -> (new_lid, loc)))
    |> List.sort_uniq ~cmp:(fun (_, l1) (_, l2) -> Location_aux.compare l1 l2)
