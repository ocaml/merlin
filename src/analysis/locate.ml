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

let last_location = ref Location.none

let {Logger. log} = Logger.for_section "locate"

type config = {
  mconfig: Mconfig.t;
  ml_or_mli: [ `ML | `MLI ];
  traverse_aliases: bool;
}

type result = {
  uid: Shape.Uid.t;
  decl_uid: Shape.Uid.t;
  file: string;
  location: Location.t;
  approximated: bool;
}

module File : sig
  type t = private
    | ML   of string
    | MLL  of string
    | MLI  of string
    | CMT  of string
    | CMTI of string

  val ml : string -> t
  val mli : string -> t
  val cmt : string -> t
  val cmti : string -> t

  val of_filename : string -> t option

  val alternate : t -> t

  val name : t -> string

  val with_ext : ?src_suffix_pair:(string * string) -> t -> string

  val explain_not_found :
    ?doc_from:string -> string -> t -> [> `File_not_found of string ]
end = struct
  type t =
    | ML   of string
    | MLL  of string
    | MLI  of string
    | CMT  of string
    | CMTI of string

  let file_path_to_mod_name f =
    Misc.unitname (Filename.basename f)

  let ml   s = ML   (file_path_to_mod_name s)
  let mll  s = MLL  (file_path_to_mod_name s)
  let mli  s = MLI  (file_path_to_mod_name s)
  let cmt  s = CMT  (file_path_to_mod_name s)
  let cmti s = CMTI (file_path_to_mod_name s)

  let of_filename fn =
    match Misc.rev_string_split ~on:'.' fn with
    | []
    | [ _ ] -> None
    | ext :: _ ->
      let ext = String.lowercase ext in
      Some (
        match ext with
        | "cmti" -> cmti fn
        | "cmt"  -> cmt fn
        | "mll"  -> mll fn
        | _ -> if Filename.check_suffix ext "i" then mli fn else ml fn
      )

  let alternate = function
    | ML  s
    | MLL s -> MLI s
    | MLI s -> ML s
    | CMT s  -> CMTI s
    | CMTI s -> CMT s

  let name = function
    | ML name
    | MLL name
    | MLI name
    | CMT name
    | CMTI name -> name

  let ext src_suffix_pair = function
    | ML _  -> fst src_suffix_pair
    | MLI _  -> snd src_suffix_pair
    | MLL _ -> ".mll"
    | CMT _ -> ".cmt"
    | CMTI _ -> ".cmti"

  let with_ext ?(src_suffix_pair=(".ml",".mli")) t =
    name t ^ ext src_suffix_pair t

  let explain_not_found ?(doc_from="") str_ident path =
    let msg =
      match path with
      | ML file ->
        sprintf "'%s' seems to originate from '%s' whose ML file could not be \
                 found" str_ident file
      | MLL file ->
        sprintf "'%s' seems to originate from '%s' whose MLL file could not be \
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

  val src : string -> File.t
  val build : string -> File.t

  val is_preferred : string -> bool
end = struct
  let prioritize_impl = ref true

  let set choice =
    prioritize_impl :=
      match choice with
      | `ML -> true
      | _ -> false

  let src   file = if !prioritize_impl then File.ml  file else File.mli  file
  let build file = if !prioritize_impl then File.cmt file else File.cmti file

  let is_preferred fn =
    match File.of_filename fn with
    | Some ML _ -> !prioritize_impl
    | Some MLI _ -> not !prioritize_impl
    | _ -> false
end

module File_switching : sig
  val reset : unit -> unit

  val move_to : digest:Digest.t -> string -> unit

  val where_am_i : unit -> string option

  val source_digest : unit -> Digest.t option
end = struct
  type t = {
    last_file_visited : string;
    digest : Digest.t;
  }

  let last_file_visited t = t.last_file_visited
  let digest t = t.digest

  let state = ref None

  let reset () = state := None

  let move_to ~digest file =
    log ~title:"File_switching.move_to" "file: %s\ndigest: %s" file
    @@ Digest.to_hex digest;

    state := Some { last_file_visited = file ; digest }

  let where_am_i () = Option.map !state ~f:last_file_visited

  let source_digest () = Option.map !state ~f:digest
end


module Utils = struct
  (* Reuse the code of [Misc.find_in_path_uncap] but returns all the files
     matching, instead of the first one. This is only used when looking for ml
     files, not cmts. Indeed for cmts we know that the load path will only ever
     contain files with uniq names; this in not the case for the "source path"
     however. We therefore get all matching files and use an heuristic at the
     call site to choose the appropriate file.

     Note: We do not refine the load path for module path as we used too. *)
  let find_all_in_path_uncap ?src_suffix_pair ~with_fallback path file =
    let name = File.with_ext ?src_suffix_pair file in
    log ~title:"find_all_in_path_uncap" "Looking for file %S in path:\n%a" name
      Logger.fmt (fun fmt -> Format.pp_print_list Format.pp_print_string fmt path);
    let uname = String.uncapitalize name in
    let fallback, ufallback =
      let alt = File.alternate file in
      let fallback = File.with_ext ?src_suffix_pair alt in
      fallback, String.uncapitalize fallback
    in
    let try_file dirname basename acc =
      if Misc.exact_file_exists ~dirname ~basename
      then Misc.canonicalize_filename (Filename.concat dirname basename) :: acc
      else acc
    in
    let try_dir acc dirname =
      let acc = try_file dirname uname acc in
      let acc = try_file dirname name acc in
      let acc =
        if with_fallback then
          let acc = try_file dirname ufallback acc in
          let acc = try_file dirname fallback acc in
          acc
        else
          acc
      in
      acc
    in
    List.fold_left ~f:try_dir ~init:[] path

  let find_all_matches ~config ?(with_fallback=false) file =
    let files =
      List.concat_map ~f:(fun synonym_pair ->
        find_all_in_path_uncap ~src_suffix_pair:synonym_pair ~with_fallback
          (Mconfig.source_path config) file
      ) Mconfig.(config.merlin.suffixes)
    in
    List.dedup_adjacent files ~cmp:String.compare

  let find_file_with_path ~config ?(with_fallback=false) file path =
    if File.name file = Misc.unitname Mconfig.(config.query.filename) then
      Some Mconfig.(config.query.filename)
    else
      let attempt_search src_suffix_pair =
        let fallback =
          if with_fallback then
            Some (File.with_ext ~src_suffix_pair (File.alternate file))
          else
            None
        in
        let fname = File.with_ext ~src_suffix_pair file in
        try Some (Misc.find_in_path_normalized ?fallback path fname)
        with Not_found -> None
      in
      try
        Some (List.find_map Mconfig.(config.merlin.suffixes) ~f:attempt_search)
      with Not_found ->
        None

  let find_file ~config ?with_fallback (file : File.t) =
    find_file_with_path ~config ?with_fallback file @@
        match file with
        | ML  _ | MLI _  | MLL _ -> Mconfig.source_path config
        | CMT _ | CMTI _         -> Mconfig.cmt_path config
end

let move_to filename cmt_infos =
  let digest =
    (* [None] only for packs, and we wouldn't have a trie if the cmt was for a
       pack. *)
    let sourcefile_in_builddir =
      Filename.concat
        (cmt_infos.Cmt_format.cmt_builddir)
        (Option.get cmt_infos.cmt_sourcefile)
    in
    match sourcefile_in_builddir |> String.split_on_char ~sep:'.' |> List.rev with
    | ext :: "pp" :: rev_path ->
      (* If the source file was a post-processed file (.pp.mli?), use the
         regular .mli? file for locate. *)
      let sourcefile_in_builddir =
        (ext :: rev_path) |> List.rev |> String.concat ~sep:"."
      in
      (match
         Misc.exact_file_exists
           ~dirname:(Filename.dirname sourcefile_in_builddir)
           ~basename:(Filename.basename sourcefile_in_builddir)
       with
       | true -> Digest.file sourcefile_in_builddir
       | false -> Option.get cmt_infos.cmt_source_digest)
    | _ -> Option.get cmt_infos.cmt_source_digest
  in
  File_switching.move_to ~digest filename

let load_cmt ~config ?(with_fallback = true) comp_unit =
  Preferences.set config.ml_or_mli;
  let file =
    Preferences.build comp_unit
  in
  match Utils.find_file ~config:config.mconfig ~with_fallback file with
  | Some path ->
      let cmt_infos = (Cmt_cache.read path).cmt_infos in
      let source_file = cmt_infos.cmt_sourcefile in
      let source_file = Option.value ~default:"*pack*" source_file in
      move_to path cmt_infos;
      Ok (source_file, cmt_infos)
  | None -> Error ()

let scrape_alias ~env ~fallback_uid ~namespace path =
  let find_type_and_uid ~env ~namespace path =
    match namespace with
    | Shape.Sig_component_kind.Module ->
      let { Types.md_type; md_uid; _ } = Env.find_module path env in
      md_type, md_uid
    | Module_type ->
      begin match Env.find_modtype path env with
      | { Types.mtd_type = Some mtd_type; mtd_uid; _ } ->
        mtd_type, mtd_uid
      | _ -> raise Not_found
    end
    | _ -> raise Not_found
  in
  let rec non_alias_declaration_uid ~fallback_uid path =
    match find_type_and_uid ~env ~namespace path with
    | Mty_alias path, fallback_uid ->
        non_alias_declaration_uid ~fallback_uid path
    | Mty_ident alias_path, fallback_uid
      when namespace = Shape.Sig_component_kind.Module_type ->
        (* This case is necessary to traverse module type aliases *)
        non_alias_declaration_uid ~fallback_uid alias_path
    | _, md_uid -> md_uid
    | exception Not_found -> fallback_uid
  in
  non_alias_declaration_uid ~fallback_uid path

type find_source_result =
  | Found of string
  | Not_found of File.t
  | Multiple_matches of string list

let find_source ~config loc =
  log ~title:"find_source" "attempt to find %S"
  loc.Location.loc_start.Lexing.pos_fname ;
  let fname = loc.Location.loc_start.Lexing.pos_fname in
  let with_fallback = loc.Location.loc_ghost in
  let file =
    match File.of_filename fname with
    | Some file -> file
    | None ->
      (* no extension? we have to decide. *)
      Preferences.src fname
  in
  let filename = File.name file in
  let initial_path =
    match File_switching.where_am_i () with
    | None -> fname
    | Some s -> s
  in
  log ~title:"find_source" "initial path: %S" initial_path;
  let dir = Filename.dirname initial_path in
  let dir =
    match config.Mconfig.query.directory with
    | "" -> dir
    | cwd -> Misc.canonicalize_filename ~cwd dir
  in
  match Utils.find_all_matches ~config ~with_fallback file with
  | [] ->
    log ~title:"find_source" "failed to find %S in source path (fallback = %b)"
       filename with_fallback ;
    log ~title:"find_source" "looking for %S in %S" (File.name file) dir ;
    begin match
      Utils.find_file_with_path ~config ~with_fallback file [dir]
    with
    | Some source -> Found source
    | None ->
      log ~title:"find_source" "Trying to find %S in %S directly" fname dir;
      try Found (Misc.find_in_path [dir] fname)
      with _ -> Not_found file
    end
  | [ x ] -> Found x
  | files ->
    log ~title:(sprintf "find_source(%s)" filename)
      "multiple matches in the source path : %s"
      (String.concat ~sep:" , " files);
    try
      match File_switching.source_digest () with
      | None ->
        log ~title:"find_source"
          "... no source digest available to select the right one" ;
        raise Not_found
      | Some digest ->
        log ~title:"find_source"
          "... trying to use source digest to find the right one" ;
        log ~title:"find_source" "Source digest: %s" (Digest.to_hex digest) ;
        Found (
          List.find files ~f:(fun f ->
            let fdigest = Digest.file f in
            log ~title:"find_source" "  %s (%s)" f (Digest.to_hex fdigest) ;
            fdigest = digest
          )
        )
    with Not_found ->
      log ~title:"find_source" "... using heuristic to select the right one" ;
      log ~title:"find_source" "we are looking for a file named %s in %s" fname dir ;
      let rev = String.reverse (Misc.canonicalize_filename ~cwd:dir fname) in
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
            match File_id.get s, File_id.get t with
            | s', t' when File_id.check s' t' ->
              0
            | _ -> n
        ) lst
      in
      match lst with
      | (i1, _) :: (i2, _) :: _ when i1 = i2 ->
        Multiple_matches files
      | (_, s) :: _ -> Found s
      | _ -> assert false

(* Well, that's just another hack.
   [find_source] doesn't like the "-o" option of the compiler. This hack handles
   Jane Street specific use case where "-o" is used to prefix a unit name by the
   name of the library which contains it. *)
let find_source ~config loc path =
  let result =
    match find_source ~config loc with
    | Found _ as result -> result
    | failure ->
      let fname = loc.Location.loc_start.Lexing.pos_fname in
      match
        let i = String.first_double_underscore_end fname in
        let pos = i + 1 in
        let fname = String.sub fname ~pos ~len:(String.length fname - pos) in
        let loc =
          let lstart = { loc.Location.loc_start with Lexing.pos_fname = fname } in
          { loc with Location.loc_start = lstart }
        in
        find_source ~config loc
      with
      | Found _ as result -> result
      | _ -> failure
      | exception _ -> failure
  in
  match (result : find_source_result) with
  | Found src -> `Found (src, loc)
  | Not_found f -> File.explain_not_found path f
  | Multiple_matches lst ->
    let matches = String.concat lst ~sep:", " in
    `File_not_found (
      sprintf "Several source files in your path have the same name, and \
               merlin doesn't know which is the right one: %s"
        matches)

(** [find_loc_of_uid] uid's location are given by tables stored int he cmt files
  for external compilation units or computed by Merlin for the current buffer.
  This function lookups a uid's location in the appropriate table. *)
let find_loc_of_uid ~config ~local_defs uid comp_unit =
  let title = "find_loc_of_uid" in
  let loc_of_decl ~uid def =
    match Misc_utils.loc_of_decl ~uid def  with
    | Some loc ->
      log ~title "Found location: %a"
        Logger.fmt (fun fmt -> Location.print_loc fmt loc.loc);
      `Some (uid, loc.loc)
    | None -> log ~title "The declaration has no location."; `None
  in
  if Env.get_unit_name () = comp_unit then begin
    log ~title "We look for %a in the current compilation unit."
      Logger.fmt (fun fmt -> Shape.Uid.print fmt uid);
    log ~title "Looking for %a in the uid_to_loc table"
      Logger.fmt (fun fmt -> Shape.Uid.print fmt uid);
    let tbl = Ast_iterators.build_uid_to_locs_tbl ~local_defs () in
    match Shape.Uid.Tbl.find_opt tbl uid with
    | Some { Location.loc; _ } -> `Some (uid, loc)
    | None -> log ~title "Uid not found in the local table."; `None
  end else begin
    log ~title "Loading the cmt file for unit %S" comp_unit;
    match load_cmt ~config comp_unit with
    | Ok (_pos_fname, cmt) ->
      log ~title "Shapes successfully loaded, looking for %a"
        Logger.fmt (fun fmt -> Shape.Uid.print fmt uid);
      begin match Shape.Uid.Tbl.find_opt cmt.cmt_uid_to_decl uid with
        | Some decl -> loc_of_decl ~uid decl
        | None -> log ~title "Uid not found in the cmt's table."; `None
      end
    | _ -> log ~title "Failed to load the cmt file"; `None
  end

let find_loc_of_comp_unit ~config uid comp_unit =
  let title = "find_loc_of_comp_unit" in
  log ~title "Got the uid of a compilation unit: %s" comp_unit;
  match load_cmt ~config comp_unit with
  | Ok (pos_fname, _cmt) ->
    let pos = Std.Lexing.make_pos ~pos_fname (1, 0) in
    let loc = { Location.loc_start=pos; loc_end=pos; loc_ghost=true } in
    `Some (uid, loc)
    | _ -> log ~title "Failed to load the CU's cmt"; `None

let find_definition_uid ~config ~env ~(decl : Env_lookup.item) path =
  let namespace = decl.namespace in
  let module Reduce = Shape_reduce.Make (struct
      let fuel = 10

      let read_unit_shape ~unit_name =
          log ~title:"read_unit_shape" "inspecting %s" unit_name;
          match
            load_cmt ~config:({config with ml_or_mli = `ML})
                     ~with_fallback:false unit_name
          with
          | Ok (filename, cmt_infos) ->
            move_to filename cmt_infos;
            log ~title:"read_unit_shape" "shapes loaded for %s" unit_name;
            cmt_infos.cmt_impl_shape
          | Error () ->
            log ~title:"read_unit_shape" "failed to find %s" unit_name;
            None
    end)
  in
  let shape = Env.shape_of_path ~namespace env path in
  log ~title:"shape_of_path" "initial: %a"
    Logger.fmt (Fun.flip Shape.print shape);
  let _keep_aliases =
    if config.traverse_aliases
    then (fun _ -> false)
    else (function
    | Shape. { uid = Some (Item { comp_unit; _ });
               desc = Alias { desc = Comp_unit alias_cu; _ };
               _ }
      when let by = comp_unit ^ "__" in
        Merlin_utils.Std.String.is_prefixed ~by alias_cu ->
      false
    | _ -> true)
  in
  let reduced = Reduce.reduce_for_uid env shape
  in
  log ~title:"shape_of_path" "reduced: %a"
    Logger.fmt (fun fmt -> Shape_reduce.print_result fmt reduced);
  reduced

let rec uid_of_result ~traverse_aliases = function
  | Shape_reduce.Resolved uid ->
      Some uid, false
  | Resolved_alias (Item { comp_unit; _ },
      (Resolved_alias (Compilation_unit comp_unit', _) as rest))
      when let by = comp_unit ^ "__" in String.is_prefixed ~by comp_unit' ->
      (* Always traverse dune-wrapper aliases *)
      uid_of_result ~traverse_aliases rest
  | Resolved_alias (_alias, rest) when traverse_aliases ->
      uid_of_result ~traverse_aliases rest
  | Resolved_alias (alias, _rest) ->
      Some alias, false
  | Unresolved { uid = Some uid; desc = Comp_unit _; approximated } ->
      Some uid, approximated
  | Approximated _ | Unresolved _ | Internal_error_missing_uid ->
      None, true

(** This is the main function here *)
let from_path ~config ~env ~local_defs ~decl path =
  let title = "from_path" in
  let unalias (decl : Env_lookup.item) =
    if not config.traverse_aliases then decl.uid else
    let namespace = decl.namespace in
    let uid = scrape_alias ~fallback_uid:decl.uid ~env ~namespace path in
    if uid <> decl.uid then
      log ~title:"uid_of_path" "Unaliased declaration uid: %a -> %a"
        Logger.fmt (Fun.flip Shape.Uid.print decl.uid)
        Logger.fmt (Fun.flip Shape.Uid.print uid);
    uid
  in
  (* Step 1:  Path => Uid *)
  let decl : Env_lookup.item = { decl with uid = (unalias decl) } in
  let uid, approximated = match config.ml_or_mli with
    | `MLI -> decl.uid, false
    | `ML ->
      let traverse_aliases = config.traverse_aliases in
      let result = find_definition_uid ~config ~env ~decl path in
      match uid_of_result ~traverse_aliases result with
      | Some uid, approx -> uid, approx
      | None, _approx ->
          log ~title "No definition uid, falling back to the declaration uid: %a"
            Logger.fmt (Fun.flip Shape.Uid.print decl.uid);
          decl.uid, true
  in
  (* Step 2:  Uid => Location *)
  let loc = match uid with
    | Predef s -> `Builtin (uid, s)
    | Internal -> `Builtin (uid, "<internal>")
    | Item {comp_unit; _} -> find_loc_of_uid ~config ~local_defs uid comp_unit
    | Compilation_unit comp_unit -> find_loc_of_comp_unit ~config uid comp_unit
  in
  let loc = match loc with
    | `None ->
      log ~title "Falling back to the declaration's location: %a"
        Logger.fmt (Fun.flip Location.print_loc decl.loc);
      `Some (decl.uid, decl.loc)
    | other -> other
  in
  (* Step 3:  Location => Source *)
  match loc with
  | `None -> assert false
  | `Builtin _ as err -> err
  | `Some (uid, loc) ->
    match find_source ~config:config.mconfig loc (Path.name path) with
    | `Found (file, location) ->
      log ~title:"find_source" "Found file: %s (%a)" file
        Logger.fmt (Fun.flip Location.print_loc location);
      `Found {
        uid;
        decl_uid = decl.uid;
        file; location; approximated }
    | `File_not_found _ as otherwise -> otherwise

let from_longident ~config ~env ~local_defs nss ident =
  let str_ident =
    try String.concat ~sep:"." (Longident.flatten ident)
    with _-> "Not a flat longident"
  in
  match Env_lookup.by_longident nss ident env with
  | None -> `Not_in_env str_ident
  | Some (path, decl) -> from_path ~config ~env ~local_defs ~decl path

let from_path ~config ~env ~local_defs ~namespace path =
  File_switching.reset ();
  match Env_lookup.by_path path namespace env with
  | None -> `Not_in_env (Path.name path)
  | Some decl -> from_path ~config ~env ~local_defs ~decl path

let infer_namespace ?namespaces ~pos lid browse is_label =
  match namespaces with
  | Some nss ->
    if not is_label
    then `Ok (nss :> Env_lookup.Namespace.inferred list)
    else if List.mem `Labels ~set:nss then (
      log ~title:"from_string" "restricting namespaces to labels";
      `Ok [ `Labels ]
    ) else (
      log ~title:"from_string"
        "input is clearly a label, but the given namespaces don't cover that";
      `Error `Missing_labels_namespace
    )
  | None ->
    match Context.inspect_browse_tree ~cursor:pos lid [browse], is_label with
    | None, _ ->
      log ~title:"from_string" "already at origin, doing nothing" ;
      `Error `At_origin
    | Some (Label _ as ctxt), true
    | Some ctxt, false ->
      log ~title:"from_string"
        "inferred context: %s" (Context.to_string ctxt);
      `Ok (Env_lookup.Namespace.from_context ctxt)
    | _, true ->
      log ~title:"from_string"
        "dropping inferred context, it is not precise enough";
      `Ok [ `Labels ]

let from_string ~config ~env ~local_defs ~pos ?namespaces path =
  File_switching.reset ();
  let browse = Mbrowse.of_typedtree local_defs in
  let lid = Type_utils.parse_longident path in
  let from_lid lid =
    let ident, is_label = Longident.keep_suffix lid in
    match infer_namespace ?namespaces ~pos lid browse is_label with
    | `Error e -> e
    | `Ok nss ->
      log ~title:"from_string"
        "looking for the source of '%s' (prioritizing %s files)"
        path (match config.ml_or_mli with `ML -> ".ml" | `MLI -> ".mli");
      from_longident ~config ~env ~local_defs nss ident
  in
  Option.value_map ~f:from_lid ~default:(`Not_found (path, None)) lid

(** When we look for docstring in external compilation unit we can perform
    a uid-based search and return the attached comment in the attributes.
    This is a more sound way to get documentation than resorting on the
    [Ocamldoc.associate_comment] heuristic *)
(* In a future release of OCaml the cmt's uid_to_loc table will contain
   fragments of the typedtree that might be used to get the docstrings without
   relying on this iteration *)
let find_doc_attributes_in_typedtree ~config ~comp_unit uid =
  let exception Found_attributes of Typedtree.attributes in
  let test elt_uid attributes =
    if Shape.Uid.equal uid elt_uid then raise (Found_attributes attributes)
  in
  let iterator =
    let first_item = ref true in
    let uid_is_comp_unit = match uid with
      | Shape.Uid.Compilation_unit _ -> true
      | _ -> false
    in
    fun env -> { Tast_iterator.default_iterator with

      (* Needed to return top-level module doc (when the uid is a compunit).
         The module docstring must be the first signature or structure item *)
      signature_item = (fun sub ({ sig_desc; _} as si) ->
        begin match sig_desc, !first_item, uid_is_comp_unit with
        | Tsig_attribute attr, true, true -> raise (Found_attributes [attr])
        | _, false, true -> raise Not_found
        | _, _, _ -> first_item := false end;
        Tast_iterator.default_iterator.signature_item sub si);

      structure_item = (fun sub ({ str_desc; _} as sti) ->
        begin match str_desc, !first_item, uid_is_comp_unit with
        | Tstr_attribute attr, true, true -> raise (Found_attributes [attr])
        | _, false, true -> raise Not_found
        | _, _, _ -> first_item := false end;
        Tast_iterator.default_iterator.structure_item sub sti);

      value_description = (fun sub ({ val_val; val_attributes; _ } as vd) ->
        test val_val.val_uid val_attributes;
        Tast_iterator.default_iterator.value_description sub vd);

      type_declaration = (fun sub ({ typ_type; typ_attributes; _ } as td) ->
        test typ_type.type_uid typ_attributes;
        Tast_iterator.default_iterator.type_declaration sub td);

      value_binding = (fun sub ({ vb_pat; vb_attributes; _ } as vb) ->
        let pat_var_iter ~f pat =
          let rec aux pat =
            let open Typedtree in
            match pat.pat_desc with
            | Tpat_var (id, _, _) -> f id
            | Tpat_alias (pat, _, _, _)
            | Tpat_variant (_, Some pat, _)
            | Tpat_lazy pat
            | Tpat_or (pat, _, _) ->
                aux pat
            | Tpat_tuple pats
            | Tpat_construct (_, _, pats, _)
            | Tpat_array pats ->
                List.iter ~f:aux pats
            | Tpat_record (pats, _) ->
                List.iter ~f:(fun (_, _, pat) -> aux pat) pats
            | _ -> ()
          in
          aux pat
        in
        pat_var_iter vb_pat ~f:(fun id ->
          try
            let vd = Env.find_value (Pident id) env in
            test vd.val_uid vb_attributes
          with Not_found -> ());
        Tast_iterator.default_iterator.value_binding sub vb)
    }
  in
  let typedtree =
    log ~title:"doc_from_uid" "Loading the cmt for unit %S" comp_unit;
    match load_cmt ~config:({config with ml_or_mli = `MLI}) comp_unit with
    | Ok (_, cmt_infos) ->
      log ~title:"doc_from_uid" "Cmt loaded, itering on the typedtree";
      begin match cmt_infos.cmt_annots with
      | Interface s -> Some (`Interface { s with
          sig_final_env = Envaux.env_of_only_summary s.sig_final_env})
      | Implementation str -> Some (`Implementation { str with
          str_final_env = Envaux.env_of_only_summary str.str_final_env})
      | _ -> None
      end
    | Error _ -> None
  in
  try begin match typedtree with
    | Some (`Interface s) ->
        let iterator = iterator s.sig_final_env in
        iterator.signature iterator s;
        log ~title:"doc_from_uid" "uid not found in the signature"
    | Some (`Implementation str) ->
        let iterator = iterator str.str_final_env in
        iterator.structure iterator str;
        log ~title:"doc_from_uid" "uid not found in the implementation"
    | _ -> () end;
    `No_documentation
  with
    | Found_attributes attrs ->
        log ~title:"doc_from_uid" "Found attributes for this uid";
        let parse_attributes attrs =
          let open Parsetree in
          try Some (List.find_map attrs ~f:(fun attr ->
            if List.exists ["ocaml.doc"; "ocaml.text"]
              ~f:(String.equal attr.attr_name.txt)
            then Ast_helper.extract_str_payload attr.attr_payload
            else None))
          with Not_found -> None
        in
        begin match parse_attributes attrs with
        | Some (doc, _) -> `Found (doc |> String.trim)
        | None -> `No_documentation end
    | Not_found -> `No_documentation

let doc_from_uid ~config ~loc uid =
  begin match uid with
  | Shape.Uid.Item { comp_unit; _ }
  | Shape.Uid.Compilation_unit comp_unit
      when Env.get_unit_name () <> comp_unit ->
        log ~title:"get_doc" "the doc (%a) you're looking for is in another
          compilation unit (%s)"
          Logger.fmt (fun fmt -> Shape.Uid.print fmt uid) comp_unit;
        (match find_doc_attributes_in_typedtree ~config ~comp_unit uid with
        | `Found doc -> `Found_doc doc
        | `No_documentation ->
            (* We fallback on the legacy heuristic to handle some unproper
               doc placement. See test [unattached-comment.t] *)
            `Found_loc loc)
  | _ ->
    (* Uid based search doesn't works in the current CU since Merlin's parser
       does not attach doc comments to the typedtree *)
    `Found_loc loc
  end

let doc_from_comment_list ~local_defs ~buffer_comments loc =
  (* When the doc we look for is in the current buffer or if search by uid
    has failed we use an alternative heuristic since Merlin's pure parser
    does not poulates doc attributes in the typedtree. *)
  let comments =
    match File_switching.where_am_i () with
    | None ->
      log ~title:"get_doc" "Using reader's comment (current buffer)";
      buffer_comments
    | Some cmt_path ->
      log ~title:"get_doc" "File switching: actually in %s" cmt_path;
      let {Cmt_cache. cmt_infos; _ } = Cmt_cache.read cmt_path in
      cmt_infos.Cmt_format.cmt_comments
  in
  log ~title:"get_doc" "%a" Logger.fmt (fun fmt ->
      Format.fprintf fmt "looking around %a inside: [\n"
        Location.print_loc !last_location;
      List.iter comments ~f:(fun (c, l) ->
          Format.fprintf fmt "  (%S, %a);\n" c
            Location.print_loc l);
      Format.fprintf fmt "]\n"
    );
  let browse = Mbrowse.of_typedtree local_defs in
  let (_, deepest_before) =
    Mbrowse.(leaf_node @@ deepest_before loc.Location.loc_start [browse])
  in
  (* based on https://v2.ocaml.org/manual/doccomments.html#ss:label-comments: *)
  let after_only = begin match deepest_before with
    | Browse_raw.Constructor_declaration _ -> true
    (* The remaining `true` cases are currently not reachable *)
    | Label_declaration _ | Record_field _ | Row_field _  -> true
    | _ -> false
  end in
  match
    Ocamldoc.associate_comment ~after_only comments loc !last_location
  with
  | None, _     -> `No_documentation
  | Some doc, _ -> `Found doc

let get_doc ~config:mconfig ~env ~local_defs ~comments ~pos =
  File_switching.reset ();
  fun path ->
  let_ref last_location Location.none @@ fun () ->
  let config = { mconfig; ml_or_mli = `MLI; traverse_aliases = true; } in
  let doc_from_uid_result =
    match path with
    | `Completion_entry (namespace, path, _loc) ->
      log ~title:"get_doc" "completion: looking for the doc of '%a'"
        Logger.fmt (fun fmt -> Path.print fmt path) ;

      let from_path =
        from_path ~config ~env ~local_defs ~namespace path
      in
      begin match from_path with
      | `Found { uid; location = loc; _ } ->
        doc_from_uid ~config ~loc uid
      | (`Builtin _ |`Not_in_env _|`File_not_found _|`Not_found _)
        as otherwise -> otherwise
      end
    | `User_input path ->
      log ~title:"get_doc" "looking for the doc of '%s'" path;
      begin match from_string ~config ~env ~local_defs ~pos path with
      | `Found { uid; location = loc; _ } ->
        doc_from_uid ~config ~loc uid
      | `At_origin ->
        `Found_loc { Location.loc_start = pos; loc_end = pos; loc_ghost = true }
      | `Missing_labels_namespace -> `No_documentation
      | (`Builtin _ | `Not_in_env _ | `Not_found _ |`File_not_found _ )
        as otherwise -> otherwise
      end
  in
  match doc_from_uid_result with
  | `Found_doc doc -> `Found doc
  | `Found_loc loc ->
      doc_from_comment_list ~local_defs ~buffer_comments:comments loc
  | `Builtin _ ->
    begin match path with
    | `User_input path -> `Builtin path
    | `Completion_entry (_, path, _) -> `Builtin (Path.name path)
    end
  | `File_not_found _
  | `Not_found _
  | `No_documentation
  | `Not_in_env _ as otherwise -> otherwise
