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
  aux [] p |> String.concat ~sep:"."

(* checks if the (un)qualified longident has a different length, i.e., has changed

   XXX(Ulugbek): computes longident length using [loc_start] and [loc_end], hence
   it doesn't work for multiline longidents because we can't compute their length *)
let same_longident new_lident { Location. loc_start; loc_end; _ } =
  let old_longident_len = Lexing.column loc_end - Lexing.column loc_start in
  loc_start.Lexing.pos_lnum = loc_end.Lexing.pos_lnum &&
  String.length new_lident = old_longident_len


let get_rewrites ~mode typer pos =
  match Mbrowse.select_open_node (Mtyper.node_at typer pos) with
  | None | Some (_, _, []) -> []
  | Some (orig_path, longident, ((_, node) :: _)) ->
    let paths =
      Browse_tree.all_occurrences_of_prefix ~strict_prefix:true orig_path node
    in
    let paths = List.concat_map ~f:snd paths in
    let leftmost_ident = Longident.flatten longident |> List.hd in
    List.filter_map paths ~f:(fun {Location. txt = path; loc} ->
      if loc.Location.loc_ghost || Location_aux.compare_pos pos loc > 0 then
        None
      else
        match qual_or_unqual_path mode leftmost_ident orig_path path with
        | s when same_longident s loc -> None
        | s -> Some (s, loc)
        | exception Not_found -> None
    )
    |> List.sort_uniq ~cmp:(fun (_,l1) (_,l2) -> Location_aux.compare l1 l2)
