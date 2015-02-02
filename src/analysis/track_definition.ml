(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2014  Frédéric Bour  <frederic.bour(_)lakaban.net>
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
open Merlin_lib


let section = Logger.section "locate"
let info_log  x = Printf.ksprintf (Logger.info  section)  x
let debug_log x = Printf.ksprintf (Logger.debug section)  x

let sources_path = Fluid.from (Misc.Path_list.of_list [])
let cfg_cmt_path = Fluid.from (Misc.Path_list.of_list [])
let loadpath     = Fluid.from (Misc.Path_list.of_list [])

let last_location = Fluid.from Location.none

let erase_loadpath ~cwd ~new_path k =
  let str_path_list =
    List.map new_path ~f:(function
      | "" ->
        (* That's the cwd at the time of the generation of the cmt, I'm
            guessing/hoping it will be the directory where we found it *)
        Logger.debugf section ~title:"loadpath" Format.pp_print_string cwd ;
        cwd
      | x ->
        Logger.debugf section ~title:"loadpath" Format.pp_print_string x ;
        x
    )
  in
  let pl = Misc.Path_list.of_string_list_ref (ref str_path_list) in
  Fluid.let' loadpath pl k

let restore_loadpath k =
  Logger.debug section ~title:"loadpath" "Restored load path" ;
  Fluid.let' loadpath (Fluid.get cfg_cmt_path) k

module Fallback = struct
  let fallback = ref `Nothing

  let get () = !fallback

  let set ~source loc =
    Logger.debugf section (fun fmt loc ->
      Format.fprintf fmt "Fallback.set %b" source;
      Location.print fmt loc
    ) loc ;
    fallback := if source then `ML loc else `MLI loc

  let reset () = fallback := `Nothing

  let is_set () = !fallback <> `Nothing
end

type filetype =
  | ML   of string
  | MLI  of string
  | CMT  of string
  | CMTI of string

module Preferences : sig
  val set : [ `ML | `MLI ] -> unit

  val cmt : string -> filetype

  val final : 'a -> [> `ML of 'a | `MLI of 'a ]
end = struct
  let prioritize_impl = ref true

  let set choice =
    prioritize_impl :=
      match choice with
      | `ML -> true
      | _ -> false

  let cmt file = if !prioritize_impl then CMT file else CMTI file

  let final file = if !prioritize_impl then `ML file else `MLI file
end

module File_switching : sig
  exception Can't_move

  val reset : unit -> unit

  val check_can_move : unit -> unit

  val move_to : ?digest:Digest.t -> string -> unit (* raises Can't_move *)

  val allow_movement : unit -> unit

  val where_am_i : unit -> string option

  val source_digest : unit -> Digest.t option
end = struct
  type t = {
    already_moved : bool ;
    last_file_visited : string option ;
    digest : Digest.t option ;
  }

  exception Can't_move

  let default =
    { already_moved = false ; last_file_visited = None ; digest = None }

  let state = ref default

  let reset () = state := default

  let check_can_move () =
    if !state.already_moved then raise Can't_move

  let move_to ?digest file =
    if !state.already_moved then raise Can't_move else
    debug_log "File_switching.move_to %s" file ;
    state := { already_moved = true ; last_file_visited = Some file ; digest }

  let allow_movement () =
    debug_log "File_switching.allow_movement" ;
    state := { !state with already_moved = false }

  let where_am_i () = !state.last_file_visited

  let source_digest () = !state.digest
end


module Utils = struct
  let is_ghost { Location. loc_ghost } = loc_ghost = true

  let longident_is_qualified = function
    | Longident.Lident _ -> false
    | _ -> true

  let file_path_to_mod_name f =
    let pref = Misc.chop_extensions f in
    String.capitalize (Filename.basename pref)

  exception File_not_found of filetype

  let filename_of_filetype = function ML name | MLI name | CMT name | CMTI name -> name
  let ext_of_filetype = function
    | ML _  -> ".ml"  | MLI _  -> ".mli"
    | CMT _ -> ".cmt" | CMTI _ -> ".cmti"

  (* Reuse the code of [Misc.find_in_path_uncap] but returns all the files
     matching, instead of the first one.
     This is only used when looking for ml files, not cmts. Indeed for cmts we
     know that the load path will only ever contain files with uniq names (in
     the presence of packed modules we refine the loadpath as we go); this in
     not the case for the "source path" however.
     We therefore get all matching files and use an heuristic at the call site
     to choose the appropriate file. *)
  let find_all_in_path_uncap ?(fallback="") path name =
    let has_fallback = fallback <> "" in
    List.map ~f:Misc.canonicalize_filename begin
      let acc = ref [] in
      let uname = String.uncapitalize name in
      let ufbck = String.uncapitalize fallback in
      let rec try_dir = function
        | List.Lazy.Nil -> !acc
        | List.Lazy.Cons (dir, rem) ->
            let fullname = Filename.concat dir name in
            let ufullname = Filename.concat dir uname in
            let ufallback = Filename.concat dir ufbck in
            if Sys.file_exists ufullname then acc := ufullname :: !acc
            else if Sys.file_exists fullname then acc := fullname :: !acc
            else if has_fallback && Sys.file_exists ufallback then
              acc := ufallback :: !acc
            else if has_fallback && Sys.file_exists fallback then
              acc := fallback :: !acc
            else
              () ;
            try_dir (Lazy.force rem)
      in try_dir (Misc.Path_list.to_list path)
    end

  let find_all_matches ?(with_fallback=false) file =
    let fname =
      Misc.chop_extension_if_any (filename_of_filetype file)
      ^ (ext_of_filetype file)
    in
    let fallback =
      if not with_fallback then "" else
      match file with
      | ML f   -> Misc.chop_extension_if_any f ^ ".mli"
      | MLI f  -> Misc.chop_extension_if_any f ^ ".ml"
      | _ -> assert false
    in
    let path = Fluid.get sources_path in
    List.uniq (find_all_in_path_uncap ~fallback path fname) ~cmp:String.compare

  let find_file_with_path ?(with_fallback=false) file path =
    let fname =
      Misc.chop_extension_if_any (filename_of_filetype file)
      ^ (ext_of_filetype file)
    in
    let fallback =
      if not with_fallback then "" else
      match file with
      | ML f   -> Misc.chop_extension_if_any f ^ ".mli"
      | MLI f  -> Misc.chop_extension_if_any f ^ ".ml"
      | CMT f  -> Misc.chop_extension_if_any f ^ ".cmti"
      | CMTI f -> Misc.chop_extension_if_any f ^ ".cmt"
    in
    try Misc.find_in_path_uncap ~fallback path fname
    with Not_found ->
      raise (File_not_found file)

  let find_file ?with_fallback file =
    find_file_with_path ?with_fallback file @@
        match file with
        | ML  _ | MLI _  -> Fluid.get sources_path
        | CMT _ | CMTI _ -> Fluid.get loadpath

  let keep_suffix =
    let open Longident in
    let rec aux = function
      | Lident str ->
        if String.lowercase str <> str then
          Some (Lident str, false)
        else
          None
      | Ldot (t, str) ->
        if String.lowercase str <> str then
          match aux t with
          | None -> Some (Lident str, true)
          | Some (t, is_label) -> Some (Ldot (t, str), is_label)
        else
          None
      | t ->
        Some (t, false) (* don't know what to do here, probably best if I do nothing. *)
    in
    function
    | Lident s -> Lident s, false
    | Ldot (t, s) ->
      begin match aux t with
      | None -> Lident s, true
      | Some (t, is_label) -> Ldot (t, s), is_label
      end
    | otherwise -> otherwise, false

  let explain_file_not_found ?(doc_from="") str_ident path =
    let msg =
      match path with
      | ML file ->
        sprintf "'%s' seems to originate from '%s' whose ML file could not be \
                 found" str_ident file
      | MLI file ->
        sprintf "'%s' seems to originate from '%s' whose MLI file could not be \
                 found" str_ident file
      | CMT file ->
        sprintf "Needed cmt file of module '%s' to locate '%s' but it is not \
                 present" file str_ident
      | CMTI file when file <> doc_from ->
        sprintf "Needed cmti file of module '%s' to locate '%s' but it is not \
                 present" file str_ident
      | CMTI _ ->
        sprintf "The documentation for '%s' originates in the current file, \
                 but no cmt is available" str_ident
    in
    `File_not_found msg
end

include Utils

type context = Type | Expr | Patt | Unknown
exception Context_mismatch

(* Remove top level indirections (i.e. Structure and Signature) and reverse
   their children so we start from the bottom of the file.
   We also remove everything appearing after [pos]: we don't want to consider
   things declared after the use point of what we are looking for. *)
let rec get_top_items ?pos browsable =
  let starts_before x =
    match pos with
    | None -> true
    | Some pos -> Lexing.compare_pos x.BrowseT.t_loc.Location.loc_start pos < 0
  in
  let ends_before x =
    match pos with
    | None -> true
    | Some pos -> Lexing.compare_pos x.BrowseT.t_loc.Location.loc_end pos < 0
  in
  List.concat_map (fun bt ->
    if not (starts_before bt) then [] else
    let open BrowseT in
    match bt.t_node with
    | Signature _
    | Structure _ ->
      let children = List.rev (Lazy.force bt.t_children) in
      if ends_before bt then children else get_top_items ?pos children
    | Signature_item _
    | Structure_item _  ->
      if ends_before bt then [ bt ] else
      let children = List.rev (Lazy.force bt.t_children) in
      get_top_items ?pos children
    | Module_binding _
    | Module_type_declaration _ ->
      (* N.B. we don't check [ends_before] here, because the fack that these are
       * not [Structure]/[Structure_item] (resp. sign) nodes means that they end
       * after [pos] and we should only consider their children. *)
      List.concat_map (Lazy.force bt.t_children) ~f:(fun bt ->
        match bt.t_node with
        | Module_expr _
        | Module_type _ ->
          (* FIXME: a bit too rough, [With_constraint] and
             [Module_type_constraint] nodes are ignored... *)
          let children = List.rev (Lazy.force bt.t_children) in
          get_top_items ?pos children
        | _ -> []
      )
    | _ -> []
  ) browsable

let repack = function
  | `Not_included -> None
  | `Mod_expr me  -> Some (BrowseT.Module_expr me)
  | `Mod_type mty -> Some (BrowseT.Module_type mty)

let get_on_track ~name item =
  match
    let open Raw_compat in
    match item.BrowseT.t_node with
    | BrowseT.Structure_item item ->
      repack (get_mod_expr_if_included ~name item),
      begin try
        let mbs = expose_module_binding item in
        let mb = List.find ~f:(fun mb -> Ident.name mb.Typedtree.mb_id = name) mbs in
        info_log "(get_on_track) %s is bound" name ;
        `Direct (BrowseT.Module_expr mb.Typedtree.mb_expr)
      with Not_found -> `Not_found end
    | BrowseT.Signature_item item ->
      repack (get_mod_type_if_included ~name item),
      begin try
        let mds = expose_module_declaration item in
        let md = List.find ~f:(fun md -> Ident.name md.Typedtree.md_id = name) mds in
        info_log "(get_on_track) %s is bound" name ;
        `Direct (BrowseT.Module_type md.Typedtree.md_type)
      with Not_found -> `Not_found end
    | _ -> assert false
  with
  | None, whatever -> whatever
  | Some thing, `Not_found ->
    info_log "(get_on_track) %s is included..." name ;
    `Included thing
  | _ -> assert false

let rec resolve_mod_alias t_node path rest =
  match
    match t_node with
    | BrowseT.Module_expr me  ->
      Raw_compat.remove_indir_me me
    | BrowseT.Module_type mty ->
      Raw_compat.remove_indir_mty mty
    | _ -> assert false (* absurd *)
  with
  | `Alias path' ->
    File_switching.allow_movement () ;
    let full_path = (Path.to_string_list path') @ path in
    Some (full_path, rest)
  | `Sg _ | `Str _ as x ->
    let lst = get_top_items (Browse.of_typer_contents [ x ]) @ rest in
    Some (path, lst)
  | `Functor msg ->
    info_log "stopping on functor%s" msg ;
    None
  | `Mod_type mod_type ->
    resolve_mod_alias (BrowseT.Module_type mod_type) path rest
  | `Mod_expr mod_expr ->
    resolve_mod_alias (BrowseT.Module_expr mod_expr) path rest
  | `Unpack ->
    (* FIXME: should we do something or stop here? *)
    info_log "found Tmod_unpack, expect random results." ;
    Some (path, rest)

let rec check_item ~source modules =
  let keep_looking = function
    | None ->
      (* Assumption: fallback is always set before calling this function *)
      `Not_found
    | Some (path, items) -> check_item ~source path items
  in
  let get_loc ~name item rest =
    let ident_locs, is_included =
      let open Raw_compat in
      match item.BrowseT.t_node with
      | BrowseT.Structure_item item ->
        str_ident_locs item, get_mod_expr_if_included item
      | BrowseT.Signature_item item ->
        sig_ident_locs item, get_mod_type_if_included item
      | _ -> assert false
    in
    try
      let res = List.assoc name ident_locs in
      Logger.debugf section (fun fmt loc ->
        Format.pp_print_string fmt "[get_loc] found at " ;
        Location.print_loc fmt loc
      ) res ;
      if source then `ML res else `MLI res
    with Not_found ->
      match repack (is_included ~name) with
      | None ->
        Fluid.let' last_location item.BrowseT.t_loc @@ fun () ->
        debug_log "[get_loc] saving last_location" ;
        check_item ~source modules rest
      | Some thing ->
        info_log "one more include to follow..." ;
        Fallback.set ~source item.BrowseT.t_loc ;
        keep_looking @@ resolve_mod_alias thing [ name ] rest
  in
  function
  | [] ->
    info_log "%s not in current file..." (String.concat ~sep:"." modules) ;
    from_path modules
  | item :: rest ->
    match modules with
    | [] -> assert false
    | [ str_ident ] -> get_loc ~name:str_ident item rest
    | mod_name :: path ->
      match get_on_track ~name:mod_name item with
      | `Not_found   ->
        Fluid.let' last_location item.BrowseT.t_loc @@ fun () ->
        debug_log "[get_on_track `Not_found] saving last_location" ;
        check_item ~source modules rest
      | `Direct me   ->
        debug_log "[get_on_track `Direct] setting fallback" ;
        Fallback.set ~source item.BrowseT.t_loc ;
        keep_looking @@ resolve_mod_alias me path rest
      | `Included me ->
        debug_log "[get_on_track `Included] setting fallback" ;
        Fallback.set ~source item.BrowseT.t_loc ;
        keep_looking @@ resolve_mod_alias me modules rest

and browse_cmts ~root modules =
  let open Cmt_format in
  let cmt_infos = Cmt_cache.read root in
  info_log "inspecting %s" root ;
  File_switching.move_to ?digest:cmt_infos.cmt_source_digest root ;
  match
    match cmt_infos.cmt_annots with
    | Interface intf      -> `Sg intf, false
    | Implementation impl -> `Str impl, true
    | Packed (_, files)   -> `Pack files, true
    | _ ->
      (* We could try to work with partial cmt files, but it'd probably fail
       * most of the time so... *)
      `Not_found, true
  with
  | `Not_found, _ -> `Not_found
  | (`Str _ | `Sg _ as typedtree), source ->
    begin match modules with
    | [] ->
      (* we were looking for a module, we found the right file, we're happy *)
      let pos = Lexing.make_pos ~pos_fname:root (1, 0) in
      let loc = { Location. loc_start=pos ; loc_end=pos ; loc_ghost=false } in
      if source then `ML loc else `MLI loc
    | _ ->
      let browses   = Browse.of_typer_contents [ typedtree ] in
      let browsable = get_top_items browses in
      check_item ~source modules browsable
    end
  | `Pack files, _ ->
    begin match modules with
    | [] -> `Not_found
    | mod_name :: modules ->
      let file = 
        List.find files ~f:(fun s -> file_path_to_mod_name s = mod_name)
      in
      File_switching.allow_movement () ;
      Logger.debug section ~title:"loadpath" "Saw packed module => erasing loadpath" ;
      let new_path = cmt_infos.cmt_loadpath in
      erase_loadpath ~cwd:(Filename.dirname root) ~new_path (fun () ->
        let root = find_file ~with_fallback:true (Preferences.cmt file) in
        browse_cmts ~root modules
      )
    end

(* The following is ugly, and deserves some explanations:
      As can be seen above, when encountering packed modules we override the
      loadpath by the one used to create the pack.
      This means that if the cmt files haven't been moved, we have access to
      the cmt file of every unit included in the pack.
      However, we might not have access to any other cmt (e.g. if others
      paths in the loadpath reference only cmis of packs).
      (Note that if we had access to other cmts, there might be conflicts,
      and the paths order would matter unless we have reliable digests...)
      Assuming we are in such a situation, if we do not find something in our
      "erased" loadpath, it could mean that we are looking for a persistent
      unit, and that's why we restore the initial loadpath. *)
and from_path path =
  File_switching.check_can_move () ;
  match path with
  | [] -> assert false
  | [ fname ] ->
    let save_digest_and_return root =
      let cmt_infos = Cmt_cache.read root in
      File_switching.move_to ?digest:cmt_infos.Cmt_format.cmt_source_digest root ;
      let pos = Lexing.make_pos ~pos_fname:fname (1, 0) in
      let loc = { Location. loc_start=pos ; loc_end=pos ; loc_ghost=true } in
      Preferences.final loc
    in
    begin try
      let cmt_file = find_file ~with_fallback:true (Preferences.cmt fname) in
      save_digest_and_return cmt_file
    with File_not_found (CMT fname | CMTI fname) ->
      restore_loadpath (fun () ->
        try
          let cmt_file = find_file ~with_fallback:true (Preferences.cmt fname) in
          save_digest_and_return cmt_file
        with File_not_found (CMT fname | CMTI fname) ->
          (* In that special case, we haven't managed to find any cmt. But we
             only need the cmt for the source digest in contains. Even if we
             don't have that we can blindly look for the source file and hope
             there are no duplicates. *)
          info_log "failed to locate the cmt[i] of '%s'" fname ;
          let pos = Lexing.make_pos ~pos_fname:fname (1, 0) in
          let loc = { Location. loc_start=pos ; loc_end=pos ; loc_ghost=true } in
          File_switching.move_to loc.Location.loc_start.Lexing.pos_fname ;
          Preferences.final loc
      )
    end
  | fname :: modules ->
    debug_log "from_path '%s'" fname ;
    try
      let cmt_file = find_file ~with_fallback:true (Preferences.cmt fname) in
      browse_cmts ~root:cmt_file modules
    with File_not_found (CMT fname | CMTI fname) as exn ->
      restore_loadpath (fun () ->
        try
          let cmt_file = find_file ~with_fallback:true (Preferences.cmt fname) in
          browse_cmts ~root:cmt_file modules
        with File_not_found (CMT fname | CMTI fname) ->
          info_log "failed to locate the cmt[i] of '%s'" fname ;
          raise exn
      )

let path_and_loc_from_label desc env =
  let open Types in
  match desc.lbl_res.desc with
  | Tconstr (path, _, _) ->
    let typ_decl = Env.find_type path env in
    path, typ_decl.Types.type_loc
  | _ -> assert false

exception Not_in_env
exception Multiple_matches of string list

let finalize_locating source loc =
  let fname = loc.Location.loc_start.Lexing.pos_fname in
  let with_fallback = loc.Location.loc_ghost in
  let mod_name = file_path_to_mod_name fname in
  let file = if source then ML mod_name else MLI mod_name in
  let filename = filename_of_filetype file in
  let full_path =
    match File_switching.where_am_i () with
    | None -> (* We have not moved, we don't want to return a filename *) None
    | Some s ->
      let dir = Filename.dirname s in
      match find_all_matches ~with_fallback file with
      | [] ->
        debug_log "failed to find \"%s\" in source path (fallback = %b)"
           filename with_fallback ;
        debug_log "looking in '%s'" dir ;
        Some (
          find_file_with_path ~with_fallback file @@
            Misc.Path_list.of_string_list_ref (ref [ dir ])
        )
      | [ x ] -> Some x
      | files ->
        info_log "multiple files named %s exist in the source path..." filename;
        try
          match File_switching.source_digest () with
          | None ->
            info_log "... no source digest available to select the right one" ;
            raise Not_found
          | Some digest ->
            info_log "... trying to use source digest to find the right one" ;
            debug_log "Source digest: %s" (Digest.to_hex digest) ;
            Some (
              List.find files ~f:(fun f ->
                let fdigest = Digest.file f in
                debug_log "  %s (%s)" f (Digest.to_hex fdigest) ;
                fdigest = digest
              )
            )
        with Not_found ->
          info_log "... using heuristic to select the right one" ;
          debug_log "we are looking for files in %s" dir ;
          let rev = String.reverse (Filename.concat dir fname) in
          let lst =
            List.map files ~f:(fun path ->
              let path' = String.reverse path in
              String.common_prefix_len rev path', path
            )
          in
          let lst =
            (* TODO: remove duplicates in [source_path] instead of using
              [sort_uniq] here. *)
            List.sort_uniq ~cmp:(fun ((i:int),s) ((j:int),t) ->
              let tmp = compare j i in
              if tmp <> 0 then tmp else
              compare s t
            ) lst
          in
          match lst with
          | (i1, s1) :: (i2, s2) :: _ when i1 = i2 ->
            raise (Multiple_matches files)
          | (_, s) :: _ -> Some s
          | _ -> assert false
  in
  `Found (full_path, loc)

let recover ~finalize ident =
  debug_log "recovering..." ;
  try
    match Fallback.get () with
    | `Nothing -> assert false
    | `ML  loc -> finalize true loc
    | `MLI loc -> finalize false loc
  with
  | File_not_found path -> explain_file_not_found ident path
  | e ->
    debug_log "recovery raised: %s" (Printexc.to_string e) ;
    raise e

let namespaces = function
  | Type        -> [ `Type ; `Constr ; `Mod ; `Modtype ; `Labels ; `Vals ]
  | Expr | Patt -> [ `Vals ; `Constr ; `Mod ; `Modtype ; `Labels ; `Type ]
  | Unknown     -> [ `Vals ; `Type ; `Constr ; `Mod ; `Modtype ; `Labels ]

exception Found of (Path.t * Location.t)

let lookup ctxt ident env =
  try
    List.iter (namespaces ctxt) ~f:(fun namespace ->
      try
        match namespace with
        | `Constr ->
          info_log "lookup in constructor namespace" ;
          let cstr_desc = Raw_compat.lookup_constructor ident env in
          raise (Found (Raw_compat.path_and_loc_of_cstr cstr_desc env))
        | `Mod ->
          info_log "lookup in module namespace" ;
          let path, _ = Raw_compat.lookup_module ident env in
          raise (Found (path, Location.symbol_gloc ()))
        | `Modtype ->
          info_log "lookup in module type namespace" ;
          let path, _ = Raw_compat.lookup_modtype ident env in
          raise (Found (path, Location.symbol_gloc ()))
        | `Type ->
          info_log "lookup in type namespace" ;
          let path, typ_decl = Env.lookup_type ident env in
          raise (Found (path, typ_decl.Types.type_loc))
        | `Vals ->
          info_log "lookup in value namespace" ;
          let path, val_desc = Env.lookup_value ident env in
          raise (Found (path, val_desc.Types.val_loc))
        | `Labels ->
          info_log "lookup in label namespace" ;
          let label_desc = Raw_compat.lookup_label ident env in
          raise (Found (path_and_loc_from_label label_desc env))
      with Not_found -> ()
    ) ;
    info_log "   ... not in the environment" ;
    raise Not_in_env
  with Found x ->
    x


let from_longident ~is_implementation ?pos ~finalize ctxt ml_or_mli lid node ancestors =
  File_switching.reset () ;
  Fallback.reset () ;
  Preferences.set ml_or_mli ;
  let env = node.BrowseT.t_env in
  let ident, is_label = keep_suffix lid in
  let str_ident = String.concat ~sep:"." (Longident.flatten ident) in
  try
    let path', loc =
      if not is_label then lookup ctxt ident env else
      (* If we know it is a record field, we only look for that. *)
      let label_desc = Raw_compat.lookup_label ident env in
      path_and_loc_from_label label_desc env
    in
    if not (is_ghost loc) then `Found (None, loc) else
      let () = debug_log
        "present in the environment, but ghost lock. walking up the typedtree."
      in
      let modules = Path.to_string_list path' in
      let items   = get_top_items ?pos ancestors in
      match check_item ~source:is_implementation modules items with
      | `Not_found when Fallback.is_set () -> recover ~finalize str_ident
      | `Not_found -> `Not_found (str_ident, File_switching.where_am_i ())
      | `ML  loc   -> finalize true loc
      | `MLI loc   -> finalize false loc
  with
  | _ when Fallback.is_set () -> recover ~finalize str_ident
  | Not_found
  | File_switching.Can't_move ->
    `Not_found (str_ident, File_switching.where_am_i ())
  | File_not_found path -> explain_file_not_found str_ident path
  | Not_in_env -> `Not_in_env str_ident
  | Multiple_matches lst ->
    let matches = String.concat lst ~sep:", " in
    `File_not_found (
      sprintf "Several source files in your path have the same name, and \
               merlin doesn't know which is the right one: %s"
        matches
    )

let inspect_pattern is_path_capitalized p =
  let open Typedtree in
  match p.pat_desc with
  | Tpat_any -> None
  | Tpat_var _ when not is_path_capitalized ->
    (* If the guard is not verified it means the pattern variable we find in
        the typedtree doesn't match the ident we reconstructed from the token
        stream.
        This should only happen in presence of a record pattern, e.g.

            { Location. loc_ghost }

        as is the case at the beginning of this file.
        However it only catches the cases where the ident we locate is prefixed
        by a module name, so not everything is handled.
        Catching everything is harder though, because we need to use the
        location to distinguish between

            { f[o]o = bar }

        and

            { foo = b[a]r }

        (where [ ] represents the cursor.)
        So err... TODO? *)
    None
  | _ -> Some Patt

let inspect_context browse path = function
  | None ->
    info_log "no position available, unable to determine context" ;
    Some Unknown
  | Some pos ->
    match browse with
    | [] ->
      Logger.infof section (fun fmt pos ->
        Format.pp_print_string fmt "no enclosing around: " ;
        Lexing.print_position fmt pos
      ) pos ;
      Some Unknown
    | node :: _ ->
      let open BrowseT in
      match node.t_node with
      | Pattern p -> 
        Logger.debugf section (fun fmt p ->
          Format.pp_print_string fmt "current node is: " ;
          Printtyped.pattern 0 fmt p
        ) p ;
        inspect_pattern (String.capitalize path = path) p
      | Value_description _
      | Type_declaration _
      | Extension_constructor _
      | Module_binding_name _
      | Module_declaration_name _ ->
        debug_log "current node is : %s" @@ BrowseT.string_of_node node.t_node ;
        None
      | Core_type _ -> Some Type
      | Expression _ -> Some Expr
      | _ ->
        Some Unknown

let from_string ~project ~is_implementation ?pos ~node ~ancestors switch path =
  let lid = Longident.parse path in
  match inspect_context (node :: ancestors) path pos with
  | None ->
    info_log "already at origin, doing nothing" ;
    `At_origin
  | Some ctxt ->
    info_log "looking for the source of '%s' (prioritizing %s files)" path
      (match switch with `ML -> ".ml" | `MLI -> ".mli") ;
    let finalize = finalize_locating in
    Fluid.let' sources_path (Project.source_path project) @@ fun () ->
    Fluid.let' cfg_cmt_path (Project.cmt_path project) @@ fun () ->
    Fluid.let' loadpath     (Project.cmt_path project) @@ fun () ->
    match
      from_longident ?pos ~is_implementation ~finalize ctxt switch lid
        node ancestors
    with
    | `Found (opt, loc) -> `Found (opt, loc.Location.loc_start)
    | `File_not_found _
    | `Not_found _
    | `Not_in_env _ as otherwise -> otherwise


let get_doc ~project ~is_implementation ?pos ~node ~ancestors source path =
  let lid    = Longident.parse path in
  Fluid.let' sources_path (Project.source_path project) @@ fun () ->
  Fluid.let' cfg_cmt_path (Project.cmt_path project) @@ fun () ->
  Fluid.let' loadpath     (Project.cmt_path project) @@ fun () ->
  Fluid.let' last_location Location.none @@ fun () ->
  match
    match inspect_context (node :: ancestors) path pos with
    | None ->
      (* We know that [pos <> None] otherwise we would have an [Unknown] ctxt. *)
      let pos = Option.get pos in
      `Found (None, { Location. loc_start=pos; loc_end=pos ; loc_ghost=true })
    | Some ctxt ->
      info_log "looking for the doc of '%s'" path ;
      let finalize _is_source loc = `Found (None, loc) in
      from_longident ?pos ~is_implementation ~finalize ctxt `MLI lid node
        ancestors
  with
  | `Found (_, loc) ->
    begin try
      let cmt_path =
        match File_switching.where_am_i () with
        | None -> find_file ~with_fallback:true (CMTI source)
        | Some cmt_path -> cmt_path
      in
      let cmt_infos = Cmt_cache.read cmt_path in
      match
        Ocamldoc.associate_comment cmt_infos.Cmt_format.cmt_comments loc
          (Fluid.get last_location)
      with
      | None, _     -> `No_documentation
      | Some doc, _ -> `Found doc
    with
    | File_not_found file -> explain_file_not_found ~doc_from:source path file
    end
  | `File_not_found _
  | `Not_found _
  | `Not_in_env _ as otherwise -> otherwise
