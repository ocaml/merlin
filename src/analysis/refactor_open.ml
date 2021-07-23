open Std

(** [leftmost_lident lid] returns the leftmost part of [lid], e.g., 
    given [String.Map.empty], [String] is returned *)
let rec leftmost_lident (lid : Longident.t) = 
  match lid with
  | Lident s -> s
  | Ldot(lid', _) -> leftmost_lident lid'
  | Lapply(_, _) -> raise @@ Invalid_argument "leftmost_lident: don't support Lapply"

(** [qual_or_unqual_path mode ~open_lident ~open_path node_path node_lid] 
    if mode is 
      `Unqualify - returns [node_lid] or [node_lid] with prefix [open_lident] cut off, 
        whichever is shorter

      `Qualify - returns [node_path] with its prefix equal to [open_lident]

    Note: by "prefix" we mean the leftmost consecutive part of a longident or a path. *)
let qual_or_unqual_path mode open_lident ~open_path node_path =
  let leftmost_open_lident = leftmost_lident open_lident in
  let rec aux acc (p : Path.t) =
    match p with
    | Pident ident ->
      Ident.name ident :: acc
    | Pdot (path', s) when
        mode = `Unqualify && Path.same open_path path' ->
      s :: acc
    | Pdot (path', s) when
        mode = `Qualify && s = leftmost_open_lident ->
      s :: acc
    | Pdot (path', s) ->
      aux (s :: acc) path'
    | _ -> raise Not_found
  in
  aux [] node_path

let same_longident new_lident old_lident =
  List.length new_lident = List.length (Longident.flatten old_lident)

let get_rewrites ~mode typer pos =
  match Mbrowse.select_open_node (Mtyper.node_at typer pos) with
  | None | Some (_, _, []) -> []
  | Some (open_path, open_lident, ((_, node) :: _)) ->
    let paths_and_lids = Browse_tree.all_occurrences_of_prefix open_path node in
    List.filter_map paths_and_lids ~f:(fun ({Location. txt = path; loc}, lid) ->
      if loc.Location.loc_ghost || Location_aux.compare_pos pos loc > 0 then
        None
      else
        match qual_or_unqual_path mode open_lident ~open_path path with
        | parts when same_longident parts lid -> None
        | parts -> Some (String.concat ~sep:"." parts, loc)
        | exception Not_found -> None
    )
    |> List.sort_uniq ~cmp:(fun (_,l1) (_,l2) -> Location_aux.compare l1 l2)
