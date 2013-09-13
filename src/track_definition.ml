open Std

let sources_path = ref []
let cwd = ref ""

module Utils = struct
  let debug_log ?prefix x = Printf.ksprintf (Logger.log `locate ?prefix) x
  let error_log x = Printf.ksprintf (Logger.error `locate) x

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

  let find_file ?(ext=".cmt") file =
    let file = String.uncapitalize file in
    let fname = Misc.chop_extension_if_any (Filename.basename file) ^ ext in
    (* FIXME: that sucks, if [cwd] = ".../_build/..." the ".ml" will exist, but
       will most likely not be the one you want to edit.
       However, just using [find_in_path_uncap] won't work either when you have
       several ml files with the same name (which can only happen in presence of packed
       modules).
       Example: scheduler.ml and raw_scheduler.ml are present in both async_core
       and async_unix. (ofc. "std.ml" is a more common example.)
      
       Note that [cwd] is set only when we have encountered a packed module, so in other
       cases [abs_cmt_file] will be something like "/file.ext" which (hopefully) won't
       exist. *)
    let abs_cmt_file = Printf.sprintf "%s/%s" !cwd fname in
    if Sys.file_exists abs_cmt_file then
      abs_cmt_file
    else
      let () =
        if !cwd <> "" then
          debug_log "%s not found. looking in source/build path..." abs_cmt_file
      in
      try Misc.find_in_path_uncap !sources_path fname
      with Not_found -> Misc.find_in_path_uncap !Config.load_path fname

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

exception Found of Location.t

let rec browse_structure browsable modules =
  (* start from the bottom *)
  let items =
    List.rev_map (fun bt ->
      let open Browse in
      match bt.context with
      | TopStructure -> Lazy.force bt.nodes
      | _ -> [bt]
    ) browsable
  in
  let rec find = function
    | [] -> None
    | item :: items -> check_item modules item (fun () -> find items)
  in
  find (List.concat items)

and check_item modules item try_next =
  let rec aux mod_item path =
    let open Browse in
    match mod_item with
    | [ { context = Module (Alias path', _) } ] ->
      let full_path = (path_to_list path') @ path in
      from_path' full_path
    | [ { context = Module (Structure, _) ; nodes } ] ->
      browse_structure (Lazy.force nodes) path
    | otherwise ->
      browse_structure otherwise path
  in
  let rec get_loc ~name item =
    match item.Browse.context with
    | Browse.Pattern (Some id, _)
    | Browse.TypeDecl (id, _) when id.Ident.name = name ->
      Some item.Browse.loc
    | Browse.Module (Browse.Named id, _) when id = name ->
      Some item.Browse.loc
    | Browse.NamedOther id when id.Ident.name = name ->
      Some item.Browse.loc
    | Browse.Module (Browse.Include ids, _)
      when List.exists ids ~f:(fun i -> i.Ident.name = name) ->
      aux (Lazy.force item.Browse.nodes) [ name ]
    | _ -> try_next ()
  in
  let get_on_track ~name item =
    match item.Browse.context with
    | Browse.Module (Browse.Named id, _) when id = name ->
      `Direct
    | Browse.Module (Browse.Include ids, _)
      when List.exists (fun i -> i.Ident.name = name) ids ->
      `Included
    | _ -> `Not_found
  in
  match modules with
  | [] -> assert false
  | [ str_ident ] -> get_loc ~name:str_ident item
  | mod_name :: path ->
    begin match
      match get_on_track ~name:mod_name item with
      | `Not_found -> None
      | `Direct -> Some path
      | `Included -> Some modules
    with
    | None -> try_next ()
    | Some path ->
      aux (Lazy.force item.Browse.nodes) path
    end

and browse_cmts ~root modules =
  let open Cmt_format in
  let cmt_infos = read_cmt root in
  match cmt_infos.cmt_annots with
  | Implementation impl ->
    let browses = Browse.structure impl in
    browse_structure browses modules
  | Packed (_, files) ->
    begin match modules with
    | [] -> assert false
    | mod_name :: modules ->
      let file = List.find files ~f:(fun f -> file_path_to_mod_name f = mod_name) in
      cwd := Filename.dirname root ;
      debug_log "Saw packed module => setting cwd to '%s'" !cwd ;
      let cmt_file = find_file file in
      browse_cmts ~root:cmt_file modules
    end
  | _ -> None (* TODO? *)

and from_path' = function
  | [] -> invalid_arg "empty path"
  | [ fname ] ->
    let pos = { Lexing. pos_fname = fname ; pos_lnum = 1 ; pos_cnum = 0 ; pos_bol = 0 } in
    Some { Location. loc_start = pos ; loc_end = pos ; loc_ghost = false }
  | fname :: modules ->
    let cmt_file = find_file fname in
    browse_cmts ~root:cmt_file modules

and from_path path = from_path' (path_to_list path)

let path_and_loc_from_cstr desc env =
  let open Types in
  match desc.cstr_tag with
  | Cstr_exception (path, loc) -> path, loc
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

let from_string ~sources ~env ~local_modules path =
  debug_log "looking for the source of '%s'" path ;
  sources_path := sources ;
  let ident, is_label = keep_suffix (Longident.parse path) in
  try
    let path, loc =
      if is_label then (
        let _, label_desc = Env.lookup_label ident env in
        path_and_loc_from_label label_desc env
      ) else (
        try
          let path, val_desc = Env.lookup_value ident env in
          path, val_desc.Types.val_loc
        with Not_found ->
        try
          let path, typ_decl = Env.lookup_type ident env in
          path, typ_decl.Types.type_loc
        with Not_found ->
        try
          let _, cstr_desc = Env.lookup_constructor ident env in
          path_and_loc_from_cstr cstr_desc env
        with Not_found ->
        try
          let path, _ = Env.lookup_module ident env in
          let loc =
            try List.assoc (Longident.last ident) local_modules
            with Not_found -> Location.symbol_gloc ()
          in
          path, loc
        with Not_found ->
          debug_log "   ... not in the environment" ;
          raise Not_found
      )
    in
    if not (is_ghost loc) then
      let fname = loc.Location.loc_start.Lexing.pos_fname in
      let full_path =
        try find_file ~ext:".ml" fname
        with Not_found ->
          error_log "   found non ghost loc but no associated ml file??" ;
          fname
      in
      Some (full_path, loc)
    else
      Option.map (from_path path) ~f:(fun loc ->
        let fname = loc.Location.loc_start.Lexing.pos_fname in
        let full_path = find_file ~ext:".ml" fname in
        full_path, loc
      )
  with Not_found ->
    None
