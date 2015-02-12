(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
                             Thomas Refis  <refis.thomas(_)gmail.com>
                             Simon Castellan  <simon.castellan(_)iuwt.fr>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

open Std
open BrowseT

open Cmt_cache
module Trie = struct
  include String.Map

  let get k t = find k t

  exception Found of string * Location.t * node

  let find f t =
    try
      iter (fun k v ->
        List.iter v ~f:(fun (l, node) ->
          if f k l node then
            raise (Found (k, l, node))
        )
      ) t ;
      raise Not_found
    with
    | Found (k, l, v) -> (k, l, v)

  let find_some f t =
    try Some (find f t)
    with Not_found -> None
end

let section = Logger.section "typedtrie"

type t = trie

type result =
  | Found of Location.t
  | Alias_of of Location.t * string list
  | Resolves_to of string list * Location.t option

let rec remove_top_indir =
  List.concat_map ~f:(fun bt ->
    match bt.t_node with
    | Signature _
    | Structure _ -> Lazy.force bt.t_children
    | _ -> [ bt ]
  )

let rec build ~trie browses =
  let rec node_for_direct_mod = function
    | `Alias path ->
      let p = Path.to_string_list path in
      Alias p
    | `Str _
    | `Sg  _ as s ->
      Internal (build ~trie:Trie.empty (Browse.of_typer_contents [s]))
    | `Mod_expr me -> node_for_direct_mod (Raw_compat.remove_indir_me me)
    | `Mod_type mty -> node_for_direct_mod (Raw_compat.remove_indir_mty mty)
    | _ ->
      Leaf
  in
  List.fold_left (remove_top_indir browses) ~init:trie ~f:(fun trie t ->
    let open BrowseT in
    match t.t_node with
    | Signature _
    | Structure _ ->
      (* Removed by [get_top_items] *)
      assert false
    | Signature_item _
    | Structure_item _ ->
      begin match
        match t.t_node with
        | Signature_item item -> Raw_compat.identify_sig_includes item
        | Structure_item item -> Raw_compat.identify_str_includes item
        | _ -> assert false
      with
      | `Not_included -> build ~trie (Lazy.force t.t_children)
      | `Included (included_idents, packed) ->
        let rec helper packed =
          let f data =
            let data = (t.t_loc, data) in
            List.fold_left included_idents ~init:trie
              ~f:(fun trie id -> Trie.add_multiple (Ident.name id) data trie)
          in
          match
            match packed with
            | `Mod_expr me -> Raw_compat.remove_indir_me  me
            | `Mod_type mt -> Raw_compat.remove_indir_mty mt
          with
          | `Alias path -> f (Included (Path.to_string_list path))
          | `Mod_type _
          | `Mod_expr _ as packed -> helper packed
          | `Unpack
          | `Functor _ -> f Leaf
          | `Str _
          | `Sg  _ as s -> build ~trie (Browse.of_typer_contents [ s ])
        in
        helper packed
      end
    | Value_binding vb ->
      let idlocs = Raw_compat.pattern_idlocs vb.Typedtree.vb_pat in
      List.fold_left idlocs ~init:trie ~f:(fun trie (id, loc) ->
        Trie.add_multiple id (loc, Leaf) trie
      )
    | Value_description vd ->
      Trie.add_multiple (Ident.name vd.Typedtree.val_id) (t.t_loc, Leaf) trie
    | Module_binding mb ->
      let node =
        node_for_direct_mod
          (Raw_compat.remove_indir_me mb.Typedtree.mb_expr)
      in
      Trie.add_multiple (Ident.name mb.Typedtree.mb_id) (t.t_loc, node) trie
    | Module_declaration md ->
      let node =
        node_for_direct_mod
          (Raw_compat.remove_indir_mty md.Typedtree.md_type)
      in
      Trie.add_multiple (Ident.name md.Typedtree.md_id) (t.t_loc, node) trie
    | Module_type_declaration mtd ->
      let node =
        match mtd.Typedtree.mtd_type with
        | None -> Leaf
        | Some m -> node_for_direct_mod (Raw_compat.remove_indir_mty m)
      in
      Trie.add_multiple (Ident.name mtd.Typedtree.mtd_id) (t.t_loc, node) trie
    | Type_declaration td ->
      (* TODO: add constructors and labels as well.
         Because why the hell not. *)
      Trie.add_multiple (Ident.name td.Typedtree.typ_id) (t.t_loc, Leaf) trie
    | Type_extension te ->
      (* TODO: add constructors and labels as well.
         Because why the hell not. *)
      Trie.add_multiple (Path.last te.Typedtree.tyext_path) (t.t_loc, Leaf) trie
    | ignored_node ->
      Logger.debugf section (fun fmt node ->
        Format.fprintf fmt "IGNORED: %s" @@ BrowseT.string_of_node node
      ) ignored_node ;
      trie
  )

let of_browses = build ~trie:Trie.empty

let rec follow ?before trie = function
  | [] -> invalid_arg "Typedtrie.follow"
  | x :: xs as path ->
    try
      let lst = Trie.get x trie in
      let lst =
        match before with
        | None -> lst
        | Some before ->
          List.filter lst ~f:(fun (l1, _) ->
            Lexing.compare_pos l1.Location.loc_start before < 0)
      in
      match
        List.sort lst ~cmp:(fun (l1, _) (l2, _) ->
          (* We wants the ones closed last to be at the beginning of the list. *)
          Lexing.compare_pos l2.Location.loc_end l1.Location.loc_end)
      with
      | [] -> Resolves_to (path, None)
      | (loc, Leaf)       :: _ ->
        (* FIXME: it seems wrong to return [Resolves_to] here.
           The prefix of the path is a leaf, anything else we might look up will
           be *wrong* *)
        if xs = [] then Found loc else Resolves_to (path, None)
      | (loc, Alias path) :: _ ->
        begin match xs with
        | [] ->
          (* FIXME: at this point, we might be deep in the trie, and [path]
             might only make sense for a few steps, but in the upper nodes it
             might need to be prefixed.
             We need to recurse like we do for [Resolves_to] *)
          Alias_of (loc, path)
        | _ -> Resolves_to (path @ xs, Some loc)
        end
      | (l, Included p) :: _ -> Resolves_to (p @ path, Some l)
      | (l, Internal t) :: _ ->
        if xs = [] then Found l else
          match follow ?before t xs with
          | Resolves_to (p, x) as checkpoint ->
            begin match follow ~before:l.Location.loc_start trie p with
            | Resolves_to (_, None) -> checkpoint
            | otherwise -> otherwise
            end
          | otherwise -> otherwise
    with
    | Not_found ->
      Resolves_to (path, None)

let rec find ~before trie path =
  match
    Trie.find_some (fun _name loc _node ->
      Lexing.compare_pos loc.Location.loc_start before < 0
      && Lexing.compare_pos loc.Location.loc_end before > 0
    ) trie
  with
  | Some (_name, loc, Internal subtrie) ->
    begin match find ~before subtrie path with
    | Resolves_to (p, x) as checkpoint ->
      begin match follow ~before:loc.Location.loc_start trie p with
      | Resolves_to (_, None) -> checkpoint
      | otherwise -> otherwise
      end
    | otherwise -> otherwise
    end
  | _ -> follow ~before trie path

let find ?before trie path =
  match before with
  | None -> follow trie path
  | Some before -> find ~before trie path

let rec dump fmt trie =
  let dump_node (loc, node) =
    match node with
    | Leaf -> Location.print_loc' fmt loc
    | Included path ->
      Format.fprintf fmt "%a <%s>" Location.print_loc' loc
        (String.concat ~sep:"." path)
    | Alias path ->
      Format.fprintf fmt "%a = %s" Location.print_loc' loc
        (String.concat ~sep:"." path)
    | Internal t ->
      Format.fprintf fmt "%a = %a" Location.print_loc' loc dump t
  in
  Format.pp_print_string fmt "{\n" ;
  Trie.iter (fun key nodes ->
    Format.fprintf fmt "%s -> " key ;
    begin match nodes with
    | [] -> assert false
    | [ x ] -> dump_node x
    | lst ->
      Format.pp_print_string fmt "[\n" ;
      List.iter lst ~f:(fun x ->
        dump_node x ;
        Format.pp_print_newline fmt ()
      )
    end ;
    Format.pp_print_newline fmt ()
  ) trie ;
  Format.pp_print_string fmt "}\n"
