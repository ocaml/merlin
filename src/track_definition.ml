let sources_path = ref []
let cwd = ref ""

module Utils = struct
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
    let fname = Misc.chop_extension_if_any (Filename.basename file) ^ ext in
    let abs_cmt_file = Printf.sprintf "%s/%s" !cwd fname in
    if Sys.file_exists abs_cmt_file then
      abs_cmt_file
    else
      try Misc.find_in_path_uncap !sources_path fname
      with Not_found -> Misc.find_in_path_uncap !Config.load_path fname

  let struct_of_mod_expr me =
    let open Typedtree in
    match me.mod_desc with
    | Tmod_ident (path, _) -> assert false
    | Tmod_structure str -> str
    | _ -> raise Not_found (* TODO *)

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

  let try_split_lident lid =
    let open Longident in
    match lid with
    | Lident _ -> None
    | Ldot (t, s) -> Some (t, Lident s)
    | Lapply _ -> invalid_arg "Lapply"
end

include Utils

exception Found of Location.t

let exists_in_sig ~ident s =
  List.exists (
    let open Types in
    function
    | Sig_value (id, _)
    | Sig_type (id, _, _)
    | Sig_exception (id, _)
    | Sig_module (id, _, _)
    | Sig_modtype (id, _)
    | Sig_class (id, _, _)
    | Sig_class_type (id, _, _) -> id.Ident.name = ident
  ) s

let rec browse_structure str modules =
  (* start from the bottom *)
  let items = List.rev str.Typedtree.str_items in
  try
    List.iter (fun item ->
      match check_item modules item with
      | None -> ()
      | Some loc -> raise (Found loc)
    ) items ;
    None
  with Found loc ->
    Some loc

and check_item modules item =
  let open Typedtree in
  let check_binding ~name (pat, _) =
    match pat.pat_desc with
    | Tpat_var (id, _loc) -> id.Ident.name = name
    | _ -> false
  in
  let rec aux mod_expr path =
    match mod_expr.mod_desc with
    | Tmod_ident (path', _) ->
      let full_path = (path_to_list path') @ path in
      from_path' full_path
    | Tmod_structure str ->
      browse_structure str path
    | Tmod_constraint (mod_expr, _, _, _) ->
      aux mod_expr path
    | Tmod_apply _
      (* Functor application, we probably want to stop here. *)
    | Tmod_unpack _ ->
      (* Unpack of a first class module, stop here as well. *)
      Some mod_expr.mod_loc
    | Tmod_functor (_,_,_,mod_expr) ->
      (* We are looking for something inside a functor definition? Weird. *)
      aux mod_expr path
  in
  let find_item ~name item =
    match item.str_desc with
    | Tstr_primitive (id, str_loc, _) when id.Ident.name = name ->
      Some str_loc.Asttypes.loc
    | Tstr_value (_, bindings) ->
      begin
        try
          let (pat, _) = List.find (check_binding ~name) bindings in
          Some pat.pat_loc
        with Not_found ->
          None
      end
    | Tstr_type lst ->
      begin
        try
          let (_,loc,_) = List.find (fun (i,_,_) -> i.Ident.name = name) lst in
          Some loc.Asttypes.loc
        with Not_found ->
          None
      end
    | Tstr_module (id, loc, _) when id.Ident.name = name ->
      Some (loc.Asttypes.loc)
    | Tstr_recmodule _ -> None (* TODO *)
    | Tstr_include (mod_expr, s) ->
      if exists_in_sig ~ident:name s then
        aux mod_expr [ name ]
      else
        None
    | _ -> None (* TODO *)
  in
  let get_mod_expr ~name item =
    match item.str_desc with
    | Tstr_module (id, _, mod_expr) when id.Ident.name = name ->
      `Direct mod_expr
    | Tstr_recmodule _ -> `Not_found (* TODO *)
    | Tstr_include (mod_expr, s) ->
      if exists_in_sig ~ident:name s then
        `Included mod_expr
      else
        `Not_found
    | _ -> `Not_found
  in
  match modules with
  | [] -> assert false
  | [ str_ident ] -> find_item ~name:str_ident item
  | mod_name :: path ->
    begin match
      match get_mod_expr ~name:mod_name item with
      | `Not_found -> None
      | `Direct mod_expr -> Some (path, mod_expr)
      | `Included mod_expr -> Some (modules, mod_expr)
    with
    | None -> None
    | Some (path, mod_expr) ->
      aux mod_expr path
    end

and browse_cmts ~root modules =
  let open Cmt_format in
  let cmt_infos = read_cmt root in
  match cmt_infos.cmt_annots with
  | Implementation impl -> browse_structure impl modules
  | Packed (_sign, files) ->
    begin match modules with
    | [] -> assert false
    | mod_name :: modules ->
      let file = List.find (fun f -> file_path_to_mod_name f = mod_name) files in
      cwd := Filename.dirname root ;
      let cmt_file = find_file file in
      browse_cmts ~root:cmt_file modules
    end
  | _ -> None (* TODO? *)

and from_path' = function
  | [] -> invalid_arg "empty path"
  | fname :: modules ->
    let cmt_file =
      let fname = (Misc.chop_extension_if_any fname) ^ ".cmt" in
      try Misc.find_in_path_uncap !sources_path fname
      with Not_found -> Misc.find_in_path_uncap !Config.load_path fname
    in
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

let from_string ~sources ~env path =
  sources_path := sources ;
  let ident, is_label = keep_suffix (Longident.parse path) in
  try
    let path, loc =
      if is_label then (
        let label_desc = Env.lookup_label ident env in
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
          let cstr_desc = Env.lookup_constructor ident env in
          path_and_loc_from_cstr cstr_desc env
      )
    in
    if not (is_ghost loc) then
      let fname = loc.Location.loc_start.Lexing.pos_fname in
      let full_path =
        try find_file ~ext:".ml" fname
        with Not_found -> fname
      in
      Some (full_path, loc)
    else
      match from_path path with
      | None -> None
      | Some loc ->
        let fname = loc.Location.loc_start.Lexing.pos_fname in
        let full_path = find_file ~ext:".ml" fname in
        Some (full_path, loc)
  with Not_found ->
    None
