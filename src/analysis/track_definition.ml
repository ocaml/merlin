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

let sources_path = Fluid.from []
let cfg_cmt_path = Fluid.from []
let loadpath     = Fluid.from []

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
  Fluid.let' loadpath str_path_list k

let restore_loadpath k =
  Logger.debug section ~title:"loadpath" "Restored load path" ;
  Fluid.let' loadpath (Fluid.get cfg_cmt_path) k

module Fallback = struct
  let fallback = ref None

  let get () = !fallback

  let set loc =
    Logger.debugf section (fun fmt loc ->
      Format.fprintf fmt "Fallback.set: " ;
      Location.print_loc fmt loc
    ) loc ;
    fallback := Some loc

  let setopt = function
    | None -> debug_log "Fallback.setopt None"
    | Some loc -> set loc

  let reset () = fallback := None

  let is_set () = !fallback <> None
end

module File = struct
  type t =
    | ML   of string
    | MLI  of string
    | CMT  of string
    | CMTI of string

  let name = function ML name | MLI name | CMT name | CMTI name -> name

  let ext = function
    | ML _  -> ".ml"  | MLI _  -> ".mli"
    | CMT _ -> ".cmt" | CMTI _ -> ".cmti"

  exception Not_found of t

  let explain_not_found ?(doc_from="") str_ident path =
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

module Preferences : sig
  val set : [ `ML | `MLI ] -> unit

  val cmt : string -> File.t
  val ml  : string -> File.t

  val is_preferred : string -> bool
end = struct
  let prioritize_impl = ref true

  let set choice =
    prioritize_impl :=
      match choice with
      | `ML -> true
      | _ -> false

  open File

  let cmt file = if !prioritize_impl then CMT file else CMTI file
  let ml file = if !prioritize_impl then ML file else MLI file

  let is_preferred filename =
    if !prioritize_impl then
      Filename.check_suffix filename "ml" ||
      Filename.check_suffix filename "ML"
    else
      Filename.check_suffix filename "mli" ||
      Filename.check_suffix filename "MLI"
end

module File_switching : sig
  val reset : unit -> unit

  val move_to : ?digest:Digest.t -> string -> unit (* raises Can't_move *)

  val where_am_i : unit -> string option

  val source_digest : unit -> Digest.t option
end = struct
  type t = {
    last_file_visited : string option ;
    digest : Digest.t option ;
  }

  let default = { last_file_visited = None ; digest = None }

  let state = ref default

  let reset () = state := default

  let move_to ?digest file =
    debug_log "File_switching.move_to %s" file ;
    state := { last_file_visited = Some file ; digest }

  let where_am_i () = !state.last_file_visited

  let source_digest () = !state.digest
end


module Utils = struct
  let is_ghost_loc { Location. loc_ghost } = loc_ghost

  let longident_is_qualified = function
    | Longident.Lident _ -> false
    | _ -> true

  let file_path_to_mod_name f =
    let pref = Misc.chop_extensions f in
    String.capitalize (Filename.basename pref)


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
    let files =
      let acc = ref [] in
      let uname = String.uncapitalize name in
      let ufbck = String.uncapitalize fallback in
      let try_dir dir =
        let fullname = Filename.concat dir name in
        let fallback = Filename.concat dir fallback in
        let ufullname = Filename.concat dir uname in
        let ufallback = Filename.concat dir ufbck in
        if Sys.file_exists ufullname then acc := ufullname :: !acc ;
        if Sys.file_exists fullname then acc := fullname :: !acc ;
        if has_fallback && Sys.file_exists ufallback then
          acc := ufallback :: !acc ;
        if has_fallback && Sys.file_exists fallback then
          acc := fallback :: !acc ;
      in
      List.iter try_dir path;
      !acc
    in
    List.map files ~f:Misc.canonicalize_filename

  let find_all_matches ?(with_fallback=false) file =
    let fname = Misc.chop_extension_if_any (File.name file) ^ (File.ext file) in
    let fallback =
      if not with_fallback then "" else
      match file with
      | File.ML f   -> Misc.chop_extension_if_any f ^ ".mli"
      | File.MLI f  -> Misc.chop_extension_if_any f ^ ".ml"
      | _ -> assert false
    in
    let path  = Fluid.get sources_path in
    let files = find_all_in_path_uncap ~fallback path fname in
    List.uniq files ~cmp:String.compare

  let find_file_with_path ?(with_fallback=false) file path =
    let fname = Misc.chop_extension_if_any (File.name file) ^ (File.ext file) in
    let fallback =
      if not with_fallback then "" else
      match file with
      | File.ML f   -> Misc.chop_extension_if_any f ^ ".mli"
      | File.MLI f  -> Misc.chop_extension_if_any f ^ ".ml"
      | File.CMT f  -> Misc.chop_extension_if_any f ^ ".cmti"
      | File.CMTI f -> Misc.chop_extension_if_any f ^ ".cmt"
    in
    try Misc.find_in_path_uncap ~fallback path fname
    with Not_found ->
      raise (File.Not_found file)

  let find_file ?with_fallback file =
    find_file_with_path ?with_fallback file @@
        match file with
        | File.ML  _ | File.MLI _  -> Fluid.get sources_path
        | File.CMT _ | File.CMTI _ -> Fluid.get loadpath
end

type context = Type | Expr | Patt | Unknown
exception Context_mismatch

let rec locate ?pos path trie =
  match Typedtrie.find ?before:pos trie path with
  | Typedtrie.Found loc -> Some loc
  | Typedtrie.Resolves_to (new_path, fallback) ->
    begin match new_path with
    | (_, `Mod) :: _ ->
      debug_log "resolves to %s" (Typedtrie.path_to_string new_path) ;
      Fallback.setopt fallback ;
      from_path new_path
    | _ ->
      debug_log "new path (%s) is not a real path. fallbacking..."
        (Typedtrie.path_to_string new_path) ;
      Logger.debugf section Typedtrie.dump trie ;
      fallback
    end
  | Typedtrie.Alias_of (loc, new_path) ->
    debug_log "alias of %s" (Typedtrie.path_to_string new_path) ;
    (* TODO: optionally follow module aliases *)
    Some loc

and browse_cmts ~root modules =
  let open Cmt_format in
  let cached = Cmt_cache.read root in
  info_log "inspecting %s" root ;
  File_switching.move_to ?digest:cached.Cmt_cache.cmt_infos.cmt_source_digest root ;
  if cached.Cmt_cache.location_trie <> String.Map.empty then
    let () = debug_log "cmt already cached" in
    locate modules cached.Cmt_cache.location_trie
  else
    match
      match cached.Cmt_cache.cmt_infos.cmt_annots with
      | Interface intf      -> `Sg intf
      | Implementation impl -> `Str impl
      | Packed (_, files)   -> `Pack files
      | _ ->
        (* We could try to work with partial cmt files, but it'd probably fail
        * most of the time so... *)
        `Not_found
    with
    | `Not_found -> None
    | (`Str _ | `Sg _ as typedtree) ->
      begin match modules with
      | [] ->
        (* we were looking for a module, we found the right file, we're happy *)
        let pos = Lexing.make_pos ~pos_fname:root (1, 0) in
        let loc = { Location. loc_start=pos ; loc_end=pos ; loc_ghost=false } in
        Some loc
      | _ ->
        let env, node =
          Browse.leaf_node @@
          match typedtree with
          | `Str str -> Browse.of_structure str
          | `Sg sg -> Browse.of_signature sg
        in
        let trie = Typedtrie.of_browses
          [BrowseT.of_node ~env node] in
        cached.Cmt_cache.location_trie <- trie ;
        locate modules trie
      end
    | `Pack files ->
      begin match modules with
      | (mod_name, `Mod) :: _ ->
        assert (List.exists files ~f:(fun s -> Utils.file_path_to_mod_name s = mod_name)) ;
        Logger.debug section ~title:"loadpath" "Saw packed module => erasing loadpath" ;
        let new_path = cached.Cmt_cache.cmt_infos.cmt_loadpath in
        erase_loadpath ~cwd:(Filename.dirname root) ~new_path (fun () ->
          from_path modules
        )
      | _ -> None
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
  debug_log "from_path '%s'" (Typedtrie.path_to_string path) ;
  match path with
  | [ fname, `Mod ] ->
    let save_digest_and_return root =
      let {Cmt_cache. cmt_infos} = Cmt_cache.read root in
      File_switching.move_to ?digest:cmt_infos.Cmt_format.cmt_source_digest root ;
      let pos = Lexing.make_pos ~pos_fname:fname (1, 0) in
      let loc = { Location. loc_start=pos ; loc_end=pos ; loc_ghost=true } in
      Some loc
    in
    begin try
      let cmt_file = Utils.find_file ~with_fallback:true (Preferences.cmt fname) in
      save_digest_and_return cmt_file
    with File.Not_found (File.CMT fname | File.CMTI fname) ->
      restore_loadpath (fun () ->
        try
          let cmt_file = Utils.find_file ~with_fallback:true (Preferences.cmt fname) in
          save_digest_and_return cmt_file
        with File.Not_found (File.CMT fname | File.CMTI fname) ->
          (* In that special case, we haven't managed to find any cmt. But we
             only need the cmt for the source digest in contains. Even if we
             don't have that we can blindly look for the source file and hope
             there are no duplicates. *)
          info_log "failed to locate the cmt[i] of '%s'" fname ;
          let pos = Lexing.make_pos ~pos_fname:fname (1, 0) in
          let loc = { Location. loc_start=pos ; loc_end=pos ; loc_ghost=true } in
          File_switching.move_to loc.Location.loc_start.Lexing.pos_fname ;
          Some loc
      )
    end
  | (fname, `Mod) :: modules ->
    begin try
      let cmt_file = Utils.find_file ~with_fallback:true (Preferences.cmt fname) in
      browse_cmts ~root:cmt_file modules
    with File.Not_found (File.CMT fname | File.CMTI fname) as exn ->
      restore_loadpath (fun () ->
        try
          let cmt_file = Utils.find_file ~with_fallback:true (Preferences.cmt fname) in
          browse_cmts ~root:cmt_file modules
        with File.Not_found (File.CMT fname | File.CMTI fname) ->
          info_log "failed to locate the cmt[i] of '%s'" fname ;
          raise exn
      )
    end
  | _ -> assert false

let path_and_loc_from_label desc env =
  let open Types in
  match desc.lbl_res.desc with
  | Tconstr (path, _, _) ->
    let typ_decl = Env.find_type path env in
    path, typ_decl.Types.type_loc
  | _ -> assert false

exception Not_in_env
exception Multiple_matches of string list

let find_source loc =
  let fname = loc.Location.loc_start.Lexing.pos_fname in
  let with_fallback = loc.Location.loc_ghost in
  let mod_name = Utils.file_path_to_mod_name fname in
  let file =
    let extensionless = Misc.chop_extension_if_any fname = fname in
    if extensionless then Preferences.ml mod_name else
    if Filename.check_suffix fname "i" then File.MLI mod_name else File.ML mod_name
  in
  let filename = File.name file in
  match File_switching.where_am_i () with
  | None -> (* We have not moved, we don't want to return a filename *) None
  | Some s ->
    let dir = Filename.dirname s in
    match Utils.find_all_matches ~with_fallback file with
    | [] ->
      debug_log "failed to find \"%s\" in source path (fallback = %b)"
          filename with_fallback ;
      debug_log "looking in '%s'" dir ;
      Some (Utils.find_file_with_path ~with_fallback file [dir])
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
            let priority = (String.common_prefix_len rev path') * 2 +
                           if Preferences.is_preferred path
                           then 1
                           else 0
            in
            priority, path
          )
        in
        let lst =
          (* TODO: remove duplicates in [source_path] instead of using
            [sort_uniq] here. *)
          List.sort_uniq ~cmp:(fun ((i:int),s) ((j:int),t) ->
            let tmp = compare j i in
            if tmp <> 0 then tmp else
            match compare s t with
            | 0 -> 0
            | n ->
              (* Check if we are referring to the same files.
                 Especially useful on OSX case-insensitive FS.
                 FIXME: May be able handle symlinks and non-existing files,
                 CHECK *)
              match Misc.file_id s, Misc.file_id t with
              | s', t' when Misc.file_id_check s' t' ->
                0
              | _ -> n
          ) lst
        in
        match lst with
        | (i1, s1) :: (i2, s2) :: _ when i1 = i2 ->
          raise (Multiple_matches files)
        | (_, s) :: _ -> Some s
        | _ -> assert false

let recover ident =
  match Fallback.get () with
  | None -> assert false
  | Some loc -> `Found loc

let namespaces = function
  | Type        -> [ `Type ; `Constr ; `Mod ; `Modtype ; `Labels ; `Vals ]
  | Expr | Patt -> [ `Vals ; `Constr ; `Mod ; `Modtype ; `Labels ; `Type ]
  | Unknown     -> [ `Vals ; `Type ; `Constr ; `Mod ; `Modtype ; `Labels ]

exception Found of (Cmt_cache.path * Location.t)

let tag namespace p = Typedtrie.tag_path ~namespace (Path.to_string_list p)

let lookup ctxt ident env =
  try
    List.iter (namespaces ctxt) ~f:(fun namespace ->
      try
        match namespace with
        | `Constr ->
          info_log "lookup in constructor namespace" ;
          let cstr_desc = Raw_compat.lookup_constructor ident env in
          let path, loc = Raw_compat.path_and_loc_of_cstr cstr_desc env in
          let path = tag `Type path in (* TODO: Use [`Constr] here *)
          raise (Found (path, loc))
        | `Mod ->
          info_log "lookup in module namespace" ;
          let path, _ = Raw_compat.lookup_module ident env in
          raise (Found (tag `Mod path, Location.symbol_gloc ()))
        | `Modtype ->
          info_log "lookup in module type namespace" ;
          let path, _ = Raw_compat.lookup_modtype ident env in
          raise (Found (tag `Modtype path, Location.symbol_gloc ()))
        | `Type ->
          info_log "lookup in type namespace" ;
          let path, typ_decl = Env.lookup_type ident env in
          raise (Found (tag `Type path, typ_decl.Types.type_loc))
        | `Vals ->
          info_log "lookup in value namespace" ;
          let path, val_desc = Env.lookup_value ident env in
          raise (Found (tag `Vals path, val_desc.Types.val_loc))
        | `Labels ->
          info_log "lookup in label namespace" ;
          let label_desc = Raw_compat.lookup_label ident env in
          let path, loc = path_and_loc_from_label label_desc env in
          let path = tag `Type path in (* TODO: Use [`Labels] here *)
          raise (Found (path, loc))
      with Not_found -> ()
    ) ;
    info_log "   ... not in the environment" ;
    raise Not_in_env
  with Found x ->
    x

let locate ~ml_or_mli ~path ~lazy_trie ~pos ~str_ident loc =
  File_switching.reset ();
  Fallback.reset ();
  Preferences.set ml_or_mli;
  try
    if not (Utils.is_ghost_loc loc) then `Found loc else
      let () =
        debug_log "present in the environment, but ghost lock.\n\
                   walking up the typedtree looking for '%s'"
          (Typedtrie.path_to_string path)
      in
      let trie = Lazy.force lazy_trie in
      match locate ~pos path trie with
      | None when Fallback.is_set () -> recover str_ident
      | None -> `Not_found (str_ident, File_switching.where_am_i ())
      | Some loc -> `Found loc
  with
  | _ when Fallback.is_set () -> recover str_ident
  | Not_found -> `Not_found (str_ident, File_switching.where_am_i ())
  | File.Not_found path -> File.explain_not_found str_ident path

(* Only used to retrieve documentation *)
let from_completion_entry ~lazy_trie ~pos (namespace, path, loc) =
  let path_lst  = Path.to_string_list path in
  let str_ident = String.concat ~sep:"." path_lst in
  let tagged_path = tag namespace path in
  locate ~ml_or_mli:`MLI ~path:tagged_path ~pos ~str_ident loc
    ~lazy_trie

let from_longident ~env ~lazy_trie ~pos ctxt ml_or_mli lid =
  let ident, is_label = Longident.keep_suffix lid in
  let str_ident = String.concat ~sep:"." (Longident.flatten ident) in
  try
    let tagged_path, loc =
      if not is_label then lookup ctxt ident env else
      (* If we know it is a record field, we only look for that. *)
      let label_desc = Raw_compat.lookup_label ident env in
      let path, loc = path_and_loc_from_label label_desc env in
      (* TODO: Use [`Labels] here *)
      tag `Type path, loc
    in
    locate ~ml_or_mli ~path:tagged_path ~lazy_trie ~pos ~str_ident loc
  with
  | Not_found -> `Not_found (str_ident, File_switching.where_am_i ())
  | Not_in_env -> `Not_in_env str_ident

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

let inspect_context browse path pos =
  match Browse.enclosing pos browse with
  | None ->
    Logger.infof section (fun fmt pos ->
      Format.pp_print_string fmt "no enclosing around: " ;
      Lexing.print_position fmt pos
    ) pos ;
    Some Unknown
  | Some enclosings ->
    let open Browse_node in
    let node = BrowseT.of_browse enclosings in
    match node.BrowseT.t_node with
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
    | Module_declaration_name _ as node ->
      debug_log "current node is : %s" @@ string_of_node node;
      None
    | Core_type _ -> Some Type
    | Expression _ -> Some Expr
    | _ ->
      Some Unknown

let from_string ~project ~env ~local_defs ~pos switch path =
  let browse = Browse.of_typer_contents local_defs in
  let lazy_trie = lazy (Typedtrie.of_browses ~local_buffer:true @@
                          List.map browse ~f:BrowseT.of_browse) in
  let lid = Longident.parse path in
  match inspect_context browse path pos with
  | None ->
    info_log "already at origin, doing nothing" ;
    `At_origin
  | Some ctxt ->
    info_log "looking for the source of '%s' (prioritizing %s files)" path
      (match switch with `ML -> ".ml" | `MLI -> ".mli") ;
    Fluid.let' sources_path (Project.source_path project) @@ fun () ->
    Fluid.let' cfg_cmt_path (Project.cmt_path project) @@ fun () ->
    Fluid.let' loadpath     (Project.cmt_path project) @@ fun () ->
    match
      from_longident ~pos ~env ~lazy_trie ctxt switch lid
    with
    | `File_not_found _ | `Not_found _ | `Not_in_env _ as error -> error
    | `Found loc ->
      try
        match find_source loc with
        | None     -> `Found (None, loc.Location.loc_start)
        | Some src -> `Found (Some src, loc.Location.loc_start)
      with
      | File.Not_found ft -> File.explain_not_found path ft
      | Multiple_matches lst ->
        let matches = String.concat lst ~sep:", " in
        `File_not_found (
          sprintf "Several source files in your path have the same name, and \
                   merlin doesn't know which is the right one: %s"
            matches
        )


let get_doc ~project ~env ~local_defs ~comments ~pos source =
  let browse = Browse.of_typer_contents local_defs in
  let lazy_trie = lazy (Typedtrie.of_browses ~local_buffer:true @@
                          List.map browse ~f:BrowseT.of_browse) in
  fun path ->
  Fluid.let' sources_path (Project.source_path project) @@ fun () ->
  Fluid.let' cfg_cmt_path (Project.cmt_path project) @@ fun () ->
  Fluid.let' loadpath     (Project.cmt_path project) @@ fun () ->
  Fluid.let' last_location Location.none @@ fun () ->
  match
    match path with
    | `Completion_entry entry -> from_completion_entry ~pos ~lazy_trie entry
    | `User_input path ->
      let lid    = Longident.parse path in
      begin match inspect_context browse path pos with
      | None -> `Found { Location. loc_start=pos; loc_end=pos ; loc_ghost=true }
      | Some ctxt ->
        info_log "looking for the doc of '%s'" path ;
        from_longident ~pos ~env ~lazy_trie ctxt `MLI lid
      end
  with
  | `Found loc ->
    let comments =
      match File_switching.where_am_i () with
      | None -> List.rev comments
      | Some cmt_path ->
        let {Cmt_cache. cmt_infos} = Cmt_cache.read cmt_path in
        cmt_infos.Cmt_format.cmt_comments
    in
    begin match
      Ocamldoc.associate_comment comments loc (Fluid.get last_location)
    with
    | None, _     -> `No_documentation
    | Some doc, _ -> `Found doc
    end
  | `File_not_found _
  | `Not_found _
  | `Not_in_env _ as otherwise -> otherwise
