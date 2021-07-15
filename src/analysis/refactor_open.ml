open Std

let qual_or_unqual_path mode leftmost_ident path p =
  let rec aux acc (p : Path.t) =
    match p with
    | Pident ident ->
      Ident.name ident :: acc
    | Pdot (path', s) when
        mode = `Unqualify && Path.same path path' ->
      s :: acc
    | Pdot (path', s) when
        mode = `Qualify && s = leftmost_ident ->
      s :: acc
    | Pdot (path', s) ->
      aux (s :: acc) path'
    | _ -> raise Not_found
  in
  aux [] p

let same_longident new_lident old_lident =
  List.length new_lident = List.length (Longident.flatten old_lident)

let get_rewrites ~mode typer pos =
  match Mbrowse.select_open_node (Mtyper.node_at typer pos) with
  | None | Some (_, _, []) -> []
  | Some (orig_path, longident, ((_, node) :: _)) ->
    let paths_and_lids = Browse_tree.all_occurrences_of_prefix orig_path node in
    let leftmost_ident = Longident.flatten longident |> List.hd in
    List.filter_map paths_and_lids ~f:(fun ({Location. txt = path; loc}, lid) ->
      if loc.Location.loc_ghost || Location_aux.compare_pos pos loc > 0 then
        None
      else
        match qual_or_unqual_path mode leftmost_ident orig_path path with
        | parts when same_longident parts lid -> None
        | parts -> Some (String.concat ~sep:"." parts, loc)
        | exception Not_found -> None
    )
    |> List.sort_uniq ~cmp:(fun (_,l1) (_,l2) -> Location_aux.compare l1 l2)
