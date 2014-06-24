open Std
open Merlin_lib

let sources_path = ref (Misc.Path_list.of_list [])
let cmt_path = ref (Misc.Path_list.of_list [])

let cwd = ref ""

module Utils = struct
  let debug_log ?prefix x = Printf.ksprintf (Logger.log `locate ?prefix) x

  let is_ghost { Location. loc_ghost } = loc_ghost = true

  let path_to_list p =
    let rec aux acc = function
      | Path.Pident id -> id.Ident.name :: acc
      | Path.Pdot (p, str, _) -> aux (str :: acc) p
      | _ -> assert false
    in
    aux [] p

  let file_path_to_mod_name f =
    let pref = Misc.chop_extensions f in
    String.capitalize (Filename.basename pref)

  type filetype =
    | ML  of string
    | CMT of string

  exception File_not_found of filetype

  let filename_of_filetype = function ML name | CMT name -> name
  let ext_of_filetype = function ML _ -> ".ml" | CMT _ -> ".cmt"

  let find_file file =
    let fname =
      (* FIXME: the [Misc.chop_extension_if_any] should have no effect here,
         make sure of that and then remove it. *)
      Misc.chop_extension_if_any (filename_of_filetype file)
      ^ (ext_of_filetype file)
    in
    (* FIXME: that sucks, if [cwd] = ".../_build/..." the ".ml" will exist, but
       will most likely not be the one you want to edit.
       However, just using [find_in_path_uncap] won't work either when you have
       several ml files with the same name (which can only happen in presence of packed
       modules).
       Example: scheduler.ml and raw_scheduler.ml are present in both async_core
       and async_unix. (ofc. "std.ml" is a more common example.)

       N.B. [cwd] is set only when we have encountered a packed module and we
       use it only when set, we don't want to look in the actual cwd of merlin
       when looking for files. *)
    try
      if !cwd = "" then raise Not_found ;
      Misc.(find_in_path_uncap (Path_list.of_string_list_ref (ref [ !cwd ])))
        fname
    with Not_found ->
    try
      let path =
        match file with
        | ML  _ -> !sources_path
        | CMT _ -> !cmt_path
      in
      Misc.find_in_path_uncap path fname
    with Not_found ->
      raise (File_not_found file)

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
end

include Utils

type result = [
  | `Found of string option * Lexing.position
  | `Not_in_env of string
  | `File_not_found of string
  | `Not_found
]

(** Reverse the list of structure items − we want to start from the bottom of
    the file − and remove top level indirections. *)
let get_top_items browsable =
  let items =
    List.rev_map (fun bt ->
      let open BrowseT in
      match bt.t_node with
      | Structure { Typedtree. str_items } -> str_items (* FIXME: rev? *)
      | Structure_item str_item -> [ str_item ]
      | _ -> [] (* TODO: handle signature *)
    ) browsable
  in
  List.concat items

let get_idents item =
  let open Typedtree in
  match item.str_desc with
  | Tstr_value (_, binding_lst) ->
    List.concat_map binding_lst ~f:(fun binding ->
      match binding.vb_pat.pat_desc with
      | Tpat_var (id, _) -> [ Ident.name id , binding.vb_loc ]
      | _ -> []
    )
  | Tstr_module mb -> [ Ident.name mb.mb_id , mb.mb_loc ]
  | Tstr_type td_list ->
    List.map td_list ~f:(fun { typ_id ; typ_loc } ->
      Ident.name typ_id, typ_loc
    )
  | Tstr_exception ec -> [ Ident.name ec.ext_id , ec.ext_loc ]
  | _ -> []

let rec check_item modules =
  let get_loc ~name item rest =
    try Some (List.assoc name (get_idents item))
    with Not_found ->
      match item.Typedtree.str_desc with
      | Typedtree.Tstr_include { Typedtree. incl_type ; incl_mod } when
        List.exists incl_type ~f:(let open Types in function
          | Sig_value (id, _)
          | Sig_type (id, _, _)
          | Sig_typext (id, _, _)
          | Sig_module (id, _, _)
          | Sig_modtype (id, _)
          | Sig_class (id, _, _)
          | Sig_class_type (id, _, _) -> Ident.name id = name
        ) ->
        debug_log "one more include to follow..." ;
        resolve_mod_alias ~fallback:item.Typedtree.str_loc incl_mod
          [ name ] rest
      | _ -> check_item modules rest
  in
  let get_on_track ~name item =
    let open Typedtree in
    match item.Typedtree.str_desc with
    | Tstr_module mb when Ident.name mb.mb_id = name ->
      debug_log "(get_on_track) %s is bound" name ;
      `Direct mb.mb_expr
    | Tstr_include { incl_type ; incl_mod } when
      List.exists incl_type ~f:(let open Types in function
        | Sig_value (id, _)
        | Sig_type (id, _, _)
        | Sig_typext (id, _, _)
        | Sig_module (id, _, _)
        | Sig_modtype (id, _)
        | Sig_class (id, _, _)
        | Sig_class_type (id, _, _) -> Ident.name id = name
      ) ->
      debug_log "(get_on_track) %s is included..." name ;
      `Included incl_mod
    | _ -> `Not_found
  in
  function
  | [] ->
    debug_log "%s not in current file..." (String.concat ~sep:"." modules) ;
    from_path' modules
  | item :: rest ->
    match modules with
    | [] -> assert false
    | [ str_ident ] -> get_loc ~name:str_ident item rest
    | mod_name :: path ->
      begin match
        match get_on_track ~name:mod_name item with
        | `Not_found -> None
        | `Direct me -> Some (path, me)
        | `Included me -> Some (modules, me)
      with
      | None -> check_item modules rest
      | Some (path, me) ->
        resolve_mod_alias ~fallback:item.Typedtree.str_loc
          me path rest
      end

and browse_cmts ~root modules =
  let open Cmt_format in
  let cmt_infos = read_cmt root in
  match cmt_infos.cmt_annots with
  | Implementation impl ->
    begin match modules with
    | [] -> (* we were looking for a module, we found the right file, we're happy *)
      let pos_fname = root in
      let pos = { Lexing. pos_fname ; pos_lnum = 1 ; pos_cnum = 0 ; pos_bol = 0 } in
      Some { Location. loc_start = pos ; loc_end = pos ; loc_ghost = false }
    | _ ->
      let browses   = Browse.of_structures [ impl ] in
      let browsable = get_top_items browses in
      check_item modules browsable
    end
  | Packed (_, files) ->
    begin match modules with
    | [] -> None
    | mod_name :: modules ->
      let file = List.(find (map files ~f:file_path_to_mod_name)) ~f:((=) mod_name) in
      cwd := Filename.dirname root ;
      debug_log "Saw packed module => setting cwd to '%s'" !cwd ;
      let cmt_file = find_file (CMT file) in
      browse_cmts ~root:cmt_file modules
    end
  | _ -> None (* TODO? *)

and from_path' ?fallback =
  let recover = function
    | None -> fallback
    | Some v -> Some v
  in
  function
  | [] -> invalid_arg "empty path"
  | [ fname ] ->
    let pos = { Lexing. pos_fname = fname ; pos_lnum = 1 ; pos_cnum = 0 ; pos_bol = 0 } in
    Some { Location. loc_start = pos ; loc_end = pos ; loc_ghost = false }
  | fname :: modules ->
    try
      let cmt_file = find_file (CMT fname) in
      recover (browse_cmts ~root:cmt_file modules)
    with
    | Not_found -> recover None
    | File_not_found (ML _) -> assert false
    | File_not_found (CMT fname) as exn ->
      match fallback with
      | None  -> raise exn
      | value -> value

and resolve_mod_alias ~fallback mod_expr path rest =
  let open Browse in
  let do_fallback = function
    | None   -> Some fallback
    | Some x -> Some x
  in
  match mod_expr.Typedtree.mod_desc with
  | Typedtree.Tmod_ident (path', _) ->
    let full_path = (path_to_list path') @ path in
    do_fallback (check_item full_path rest)
  | Typedtree.Tmod_structure str ->
    let browsable = get_top_items (Browse.of_structures [ str ]) @ rest in
    do_fallback (check_item path browsable)
  | Typedtree.Tmod_functor _
  | Typedtree.Tmod_apply (_, _, _) ->
    (* We don't want to follow functors instantiation *)
    debug_log "stopping on functor instantiation" ;
    Some (mod_expr.Typedtree.mod_loc)
  | Typedtree.Tmod_constraint (mod_expr, _, _, _) ->
    resolve_mod_alias ~fallback mod_expr path rest
  | Typedtree.Tmod_unpack _ ->
    do_fallback (check_item path rest)

let path_and_loc_from_cstr desc env =
  let open Types in
  match desc.cstr_tag with
  | Cstr_extension (path, loc) -> path, desc.cstr_loc
  | _ ->
    match desc.cstr_res.desc with
    | Tconstr (path, _, _) ->
      let typ_decl = Env.find_type path env in
      path, typ_decl.Types.type_loc
    | _ -> assert false

let path_and_loc_from_label desc env =
  let open Types in
  match desc.lbl_res.desc with
  | Tconstr (path, _, _) ->
    let typ_decl = Env.find_type path env in
    path, typ_decl.Types.type_loc
  | _ -> assert false

exception Not_in_env

let from_string ~project ~env ~local_defs path =
  cwd := "" (* Reset the cwd before doing anything *) ;
  sources_path := Project.source_path project ;
  cmt_path := Project.cmt_path project ;
  debug_log "looking for the source of '%s'" path ;
  let ident, is_label = keep_suffix (Longident.parse path) in
  let str_ident = String.concat ~sep:"." (Longident.flatten ident) in
  try
    let path, loc =
      if is_label then
        let label_desc = Typing_aux.lookup_label ident env in
        path_and_loc_from_label label_desc env
      else (
        try
          let path, val_desc = Env.lookup_value ident env in
          path, val_desc.Types.val_loc
        with Not_found ->
        try
          let path, typ_decl = Env.lookup_type ident env in
          path, typ_decl.Types.type_loc
        with Not_found ->
        try
          let cstr_desc = Typing_aux.lookup_constructor ident env in
          path_and_loc_from_cstr cstr_desc env
        with Not_found ->
        try
          let path, _ = Merlin_types_custom.lookup_module ident env in
          path, Location.symbol_gloc ()
        with Not_found ->
          debug_log "   ... not in the environment" ;
          raise Not_in_env
      )
    in
    if not (is_ghost loc) then
      `Found (None, loc.Location.loc_start)
    else
      let opt =
        let modules = path_to_list path in
        (* looks like local_defs is already in reversed order. So we need to
            reverse it here (since [get_browsable] is going to reverse it one
            last time). *)
        let local_defs = Browse.of_structures local_defs in
        check_item modules (get_top_items local_defs)
      in
      match opt with
      | None -> `Not_found
      | Some loc ->
        let fname = loc.Location.loc_start.Lexing.pos_fname in
        let full_path = find_file (ML (file_path_to_mod_name fname)) in
        `Found (Some full_path, loc.Location.loc_start)
  with
  | Not_found -> `Not_found
  | File_not_found path -> 
    let msg =
      match path with
      | ML file ->
        Printf.sprintf "'%s' seems to originate from '%s' which could not be found"
          str_ident file
      | CMT file ->
        Printf.sprintf "Needed cmt file of module '%s' to locate '%s' but it is not present"
          file str_ident
    in
    `File_not_found msg
  | Not_in_env -> `Not_in_env str_ident
