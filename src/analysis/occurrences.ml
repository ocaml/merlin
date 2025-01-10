open Std
module Lid_set = Index_format.Lid_set

let { Logger.log } = Logger.for_section "occurrences"

type t =
  { locs : Warnings.loc list; status : Query_protocol.occurrences_status }

let () = Mtyper.set_index_items Index_occurrences.items

let set_fname ~file (loc : Location.t) =
  let pos_fname = file in
  { loc with
    loc_start = { loc.loc_start with pos_fname };
    loc_end = { loc.loc_end with pos_fname }
  }

(* A longident can have the form: A.B.x Right now we are only interested in
   values, but we will eventually want to index all occurrences of modules in
   such longidents. However there is an issue with that: we only have the
   location of the complete longident which might span multiple lines. This is
   enough to get the last component since it will always be on the last line,
   but will prevent us to find the location of previous components.

   However, we can safely deduce the location of the last part of the lid only
   when the ident does not require parenthesis. In that case the loc sie differs
   from the name size in a way that depends on the concrete syntax which is
   lost. *)
let last_loc (loc : Location.t) lid =
  match lid with
  | Longident.Lident _ -> loc
  | _ ->
    let last_segment = Longident.last lid in
    let needs_parens = Pprintast.needs_parens ~kind:Other last_segment in
    if not needs_parens then
      let last_size = last_segment |> String.length in
      { loc with
        loc_start =
          { loc.loc_end with pos_cnum = loc.loc_end.pos_cnum - last_size }
      }
    else loc

let uid_and_loc_of_node env node =
  let open Browse_raw in
  log ~title:"occurrences" "Looking for uid of node %s" @@ string_of_node node;
  match node with
  | Module_binding_name { mb_id = Some ident; mb_name; _ } ->
    let md = Env.find_module (Pident ident) env in
    Some (md.md_uid, mb_name.loc)
  | Pattern
      { pat_desc = Tpat_var (_, name, uid) | Tpat_alias (_, _, name, uid); _ }
    -> Some (uid, name.loc)
  | Type_declaration { typ_type; typ_name; _ } ->
    Some (typ_type.type_uid, typ_name.loc)
  | Label_declaration { ld_uid; ld_name; _ } -> Some (ld_uid, ld_name.loc)
  | Constructor_declaration { cd_uid; cd_name; _ } -> Some (cd_uid, cd_name.loc)
  | Value_description { val_val; val_name; _ } ->
    Some (val_val.val_uid, val_name.loc)
  | _ -> None

let comp_unit_of_uid = function
  | Shape.Uid.Compilation_unit comp_unit | Item { comp_unit; _ } ->
    Some comp_unit
  | Internal | Predef _ -> None

module Stat_check : sig
  type t
  val create : cache_size:int -> Index_format.index -> t
  val check : t -> file:string -> bool
  val get_outdated_files : t -> String.Set.t
end = struct
  type t = { index : Index_format.index; cache : (string, bool) Hashtbl.t }

  let create ~cache_size index = { index; cache = Hashtbl.create cache_size }

  let get_outdated_files t =
    Hashtbl.fold
      (fun file check acc -> if check then acc else String.Set.add file acc)
      t.cache String.Set.empty

  let stat t file =
    let open Index_format in
    match Stats.find_opt file t.index.stats with
    | None ->
      log ~title:"stat_check" "No stats found for file %S." file;
      true
    | Some { size; _ } -> (
      try
        let stats = Unix.stat file in
        let equal =
          (* This is fast but approximative. A better option would be to check
             [mtime] and then [source_digest] if the times differ. *)
          Int.equal stats.st_size size
        in
        if not equal then
          log ~title:"stat_check"
            "File %s has been modified since the index was built." file;
        equal
      with Unix.Unix_error _ ->
        log ~title:"stat_check" "Could not stat file %S" file;
        false)

  let check t ~file =
    let cache_and_return b =
      Hashtbl.add t.cache file b;
      b
    in
    match Hashtbl.find_opt t.cache file with
    | Some result -> result
    | None -> cache_and_return (stat t file)
end

let get_buffer_locs result uid =
  Stamped_hashtable.fold
    (fun (uid', loc) () acc ->
      if Shape.Uid.equal uid uid' then Lid_set.add loc acc else acc)
    (Mtyper.get_index result) Lid_set.empty

let get_external_locs ~(config : Mconfig.t) ~current_buffer_path uid =
  let title = "get_external_locs" in
  List.filter_map config.merlin.index_files ~f:(fun file ->
      log ~title "Lookin for occurrences of %a in index %s" Logger.fmt
        (Fun.flip Shape.Uid.print uid)
        file;
      let external_locs =
        try
          let external_index = Index_cache.read file in
          Index_format.Uid_map.find_opt uid external_index.defs
          |> Option.map ~f:(fun uid_locs -> (external_index, uid_locs))
        with Index_format.Not_an_index _ | Sys_error _ ->
          log ~title "Could not load index %s" file;
          None
      in
      Option.map external_locs ~f:(fun (index, locs) ->
          let stats = Stat_check.create ~cache_size:128 index in
          ( Lid_set.filter
              (fun { loc; _ } ->
                (* We ignore external results that concern the current buffer *)
                let file = loc.Location.loc_start.Lexing.pos_fname in
                let file, buf =
                  match config.merlin.source_root with
                  | Some root -> (Filename.concat root file, current_buffer_path)
                  | None -> (file, config.query.filename)
                in
                let file = Misc.canonicalize_filename file in
                let buf = Misc.canonicalize_filename buf in
                if String.equal file buf then false
                else begin
                  (* We ignore external results if their source was modified *)
                  let check = Stat_check.check stats ~file in
                  if not check then
                    log ~title "File %s might be out-of-sync." file;
                  check
                end)
              locs,
            Stat_check.get_outdated_files stats )))

let lookup_related_uids_in_indexes ~(config : Mconfig.t) uid =
  let title = "lookup_related_uids_in_indexes" in
  let open Index_format in
  let related_uids =
    List.fold_left ~init:Uid_map.empty config.merlin.index_files
      ~f:(fun acc index_file ->
        try
          let index = Index_cache.read index_file in
          Uid_map.union
            (fun _ a b -> Some (Union_find.union ~f:Uid_set.union a b))
            index.related_uids acc
        with Index_format.Not_an_index _ | Sys_error _ ->
          log ~title "Could not load index %s" index_file;
          acc)
  in
  Uid_map.find_opt uid related_uids
  |> Option.value_map ~default:[] ~f:(fun x ->
         x |> Union_find.get |> Uid_set.to_list)

let find_linked_uids ~config ~scope ~name uid =
  let title = "find_linked_uids" in
  match uid with
  | Shape.Uid.Item { from = _; comp_unit; _ } ->
    let locate_config =
      { Locate.mconfig = config; ml_or_mli = `ML; traverse_aliases = false }
    in
    let check_name uid =
      Locate.lookup_uid_decl ~config uid
      |> Option.bind ~f:(Typedtree_utils.location_of_declaration ~uid)
      |> Option.value_map
           ~f:(fun { Location.txt; _ } ->
             let result = String.equal name txt in
             if not result then
               log ~title "Found clashing idents %S <> %S. Ignoring UID %a."
                 name txt Logger.fmt
                 (Fun.flip Shape.Uid.print uid);
             result)
           ~default:false
    in
    let related_uids =
      match scope with
      | `Buffer -> []
      | `Project -> Locate.get_linked_uids ~config:locate_config ~comp_unit uid
      | `Renaming -> lookup_related_uids_in_indexes ~config uid
    in
    log ~title "Found related uids: [%a]" Logger.fmt (fun fmt ->
        List.iter ~f:(fprintf fmt "%a;" Shape.Uid.print) related_uids);
    List.filter ~f:check_name related_uids
  | _ -> []

let locs_of ~config ~env ~typer_result ~pos ~scope path =
  log ~title:"occurrences" "Looking for occurences of %s (pos: %s)" path
    (Lexing.print_position () pos);
  let local_defs = Mtyper.get_typedtree typer_result in
  let locate_result =
    Locate.from_string
      ~config:{ mconfig = config; traverse_aliases = false; ml_or_mli = `ML }
      ~env ~local_defs ~pos path
  in
  (* When we fail to find an exact definition we restrict scope to `Buffer *)
  let def, scope =
    match locate_result with
    | `At_origin ->
      log ~title:"locs_of" "Cursor is on definition / declaration";
      (* We are on  a definition / declaration so we look for the node's uid  *)
      let browse = Mbrowse.of_typedtree local_defs in
      let env, node = Mbrowse.leaf_node (Mbrowse.enclosing pos [ browse ]) in
      let node_uid_loc = uid_and_loc_of_node env node in
      (node_uid_loc, scope)
    | `Found { uid; location; approximated = false; _ }
    | `File_not_found { uid; location; approximated = false; _ } ->
      log ~title:"locs_of" "Found definition uid using locate: %a " Logger.fmt
        (fun fmt -> Shape.Uid.print fmt uid);
      (Some (uid, location), scope)
    | `Found { decl_uid; location; approximated = true; _ }
    | `File_not_found { decl_uid; location; approximated = true; _ } ->
      log ~title:"locs_of" "Approx. definition: %a " Logger.fmt (fun fmt ->
          Shape.Uid.print fmt decl_uid);
      (Some (decl_uid, location), `Buffer)
    | `Builtin (uid, s) ->
      log ~title:"locs_of" "Locate found a builtin: %s" s;
      (Some (uid, Location.none), scope)
    | _ ->
      log ~title:"locs_of" "Locate failed to find a definition.";
      (None, `Buffer)
  in
  let current_buffer_path =
    Filename.concat config.query.directory config.query.filename
  in
  match def with
  | Some (def_uid, def_loc) ->
    log ~title:"locs_of" "Definition has uid %a (%a)" Logger.fmt
      (fun fmt -> Shape.Uid.print fmt def_uid)
      Logger.fmt
      (fun fmt -> Location.print_loc fmt def_loc);
    log ~title:"locs_of" "Indexing current buffer";
    let buffer_locs = get_buffer_locs typer_result def_uid in
    let external_locs =
      if scope = `Buffer then []
      else
        let name =
          String.split_on_char ~sep:'.' path |> List.last |> Option.get
        in
        let additional_uids = find_linked_uids ~config ~scope ~name def_uid in
        List.concat_map
          (def_uid :: additional_uids)
          ~f:(get_external_locs ~config ~current_buffer_path)
    in
    let external_locs, out_of_sync_files =
      List.fold_left
        ~init:(Lid_set.empty, String.Set.empty)
        ~f:(fun (acc_locs, acc_files) (locs, files) ->
          (Lid_set.union acc_locs locs, String.Set.union acc_files files))
        external_locs
    in
    let locs = Lid_set.union buffer_locs external_locs in
    (* Some of the paths may have redundant `.`s or `..`s in them. Although canonicalizing
       is not necessary for correctness, it makes the output a bit nicer. *)
    let canonicalize_file_in_loc ({ txt; loc } : 'a Location.loc) :
        'a Location.loc =
      let file =
        Misc.canonicalize_filename ?cwd:config.merlin.source_root
          loc.loc_start.pos_fname
      in
      { txt; loc = set_fname ~file loc }
    in
    let locs = Lid_set.map canonicalize_file_in_loc locs in
    let locs =
      log ~title:"occurrences" "Found %i locs" (Lid_set.cardinal locs);
      Lid_set.elements locs
      |> List.filter_map ~f:(fun { Location.txt; loc } ->
             let lid = try Longident.head txt with _ -> "not flat lid" in
             log ~title:"occurrences" "Found occ: %s %a" lid Logger.fmt
               (Fun.flip Location.print_loc loc);
             let loc = last_loc loc txt in
             let fname = loc.Location.loc_start.Lexing.pos_fname in
             if not (Filename.is_relative fname) then Some loc
             else
               match config.merlin.source_root with
               | Some path ->
                 let file = Filename.concat path loc.loc_start.pos_fname in
                 Some (set_fname ~file loc)
               | None -> begin
                 match Locate.find_source ~config loc fname with
                 | `Found (file, _) -> Some (set_fname ~file loc)
                 | `File_not_found msg ->
                   log ~title:"occurrences" "%s" msg;
                   None
               end)
    in
    let def_uid_is_in_current_unit =
      let uid_comp_unit = comp_unit_of_uid def_uid in
      Option.value_map ~default:false uid_comp_unit
        ~f:(String.equal @@ Env.get_current_unit_name ())
    in
    let status =
      match (scope, String.Set.to_list out_of_sync_files) with
      | `Buffer, _ -> `Not_requested
      | _, [] -> `Included
      | _, l -> `Out_of_sync l
    in
    if not def_uid_is_in_current_unit then { locs; status }
    else
      let locs = set_fname ~file:current_buffer_path def_loc :: locs in
      { locs; status }
  | None -> { locs = []; status = `No_def }
