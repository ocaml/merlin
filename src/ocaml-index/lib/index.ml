module Kind = Shape.Sig_component_kind
open Index_format

let with_root ?root file =
  match root with
  | None -> file
  | Some root -> Filename.concat root file

let add_root ~root (lid : Longident.t Location.loc) =
  match root with
  | None -> lid
  | Some root ->
    let pos_fname = Filename.concat root lid.loc.loc_start.pos_fname in
    { lid with
      loc =
        { lid.loc with
          loc_start = { lid.loc.loc_start with pos_fname };
          loc_end = { lid.loc.loc_end with pos_fname }
        }
    }

let merge m m' =
  Uid_map.union (fun _uid locs locs' -> Some (Lid_set.union locs locs')) m m'

let add_one uid lid map =
  let lid = Lid.of_lid lid in
  Uid_map.update uid
    (function
      | None -> Some (Lid_set.singleton lid)
      | Some set -> Some (Lid_set.add lid set))
    map

(** Cmt files contains a table of declarations' Uids associated to a typedtree
    fragment. [add_locs_from_fragments] gather locations from these *)
let gather_locs_from_fragments ~root ~rewrite_root map fragments =
  let to_located_lid (name : string Location.loc) =
    { name with txt = Longident.Lident name.txt }
  in
  let add_loc uid fragment acc =
    match Typedtree_utils.location_of_declaration ~uid fragment with
    | None -> acc
    | Some lid ->
      let lid = to_located_lid lid in
      let lid = if rewrite_root then add_root ~root lid else lid in
      add_one uid lid acc
  in
  Shape.Uid.Tbl.fold add_loc fragments map

module Reduce_conf = struct
  let fuel = 10

  let try_load ~unit_name () =
    let cmt = Format.sprintf "%s.cmt" unit_name in
    match Cmt_cache.read (Load_path.find_normalized cmt) with
    | cmt_item ->
      Log.debug "Loaded CMT %s" cmt;
      cmt_item.cmt_infos.cmt_impl_shape
    | exception Not_found ->
      Log.warn "Failed to load file %S in load_path: @[%s@]\n%!" cmt
      @@ String.concat "; " (Load_path.get_path_list ());
      None

  let read_unit_shape ~unit_name =
    Log.debug "Read unit shape: %s\n%!" unit_name;
    try_load ~unit_name ()
end

let init_load_path_once ~do_not_use_cmt_loadpath =
  let loaded = ref false in
  fun ~(dirs : Load_path.paths) cmt_loadpath ->
    if not !loaded then (
      let cmt_visible, cmt_hidden =
        if do_not_use_cmt_loadpath then ([], [])
        else (cmt_loadpath.Load_path.visible, cmt_loadpath.Load_path.hidden)
      in
      let visible = List.concat [ cmt_visible; dirs.visible ] in
      let hidden = List.concat [ cmt_hidden; dirs.hidden ] in
      Load_path.(init ~auto_include:no_auto_include ~visible ~hidden);
      loaded := true)

let index_of_cmt ~into ~root ~rewrite_root ~build_path ~do_not_use_cmt_loadpath
    ~store_shapes cmt_infos =
  let { Cmt_format.cmt_loadpath;
        cmt_impl_shape;
        cmt_modname;
        cmt_uid_to_decl;
        cmt_ident_occurrences;
        cmt_initial_env;
        cmt_sourcefile;
        cmt_source_digest;
        cmt_declaration_dependencies;
        _
      } =
    cmt_infos
  in
  init_load_path_once ~do_not_use_cmt_loadpath ~dirs:build_path cmt_loadpath;
  let module Reduce = Shape_reduce.Make (Reduce_conf) in
  let defs =
    gather_locs_from_fragments ~root ~rewrite_root into.defs cmt_uid_to_decl
  in
  (* The list [cmt_ident_occurrences] associate each ident usage location in the
     module with its (partially) reduced shape. We finish the reduction and
     group together all the locations that share the same definition uid. *)
  let defs, approximated =
    List.fold_left
      (fun ((acc_defs, acc_apx) as acc) (lid, (item : Shape_reduce.result)) ->
        let lid = if rewrite_root then add_root ~root lid else lid in
        let resolved =
          match item with
          | Unresolved shape -> Reduce.reduce_for_uid cmt_initial_env shape
          | result -> result
        in
        match Locate.uid_of_result ~traverse_aliases:false resolved with
        | Some uid, false -> (add_one uid lid acc_defs, acc_apx)
        | Some uid, true -> (acc_defs, add_one uid lid acc_apx)
        | None, _ -> acc)
      (defs, into.approximated) cmt_ident_occurrences
  in
  let cu_shape = into.cu_shape in
  if store_shapes then
    Option.iter (Hashtbl.add cu_shape cmt_modname) cmt_impl_shape;
  let stats =
    match cmt_sourcefile with
    | None -> into.stats
    | Some src -> (
      let rooted_src = with_root ?root src in
      try
        let stats = Unix.stat rooted_src in
        let src = if rewrite_root then rooted_src else src in
        Stats.add src
          { mtime = stats.st_mtime;
            size = stats.st_size;
            source_digest = cmt_source_digest
          }
          into.stats
      with Unix.Unix_error _ -> into.stats)
  in
  let related_uids =
    List.fold_left
      (fun acc (_, uid1, uid2) ->
        let union = Union_find.make (Uid_set.of_list [ uid1; uid2 ]) in
        let map_update uid =
          Uid_map.update uid (function
            | None -> Some union
            | Some union' -> Some (Union_find.union union' union))
        in
        acc |> map_update uid1 |> map_update uid2)
      into.related_uids cmt_declaration_dependencies
  in
  { defs;
    approximated;
    cu_shape;
    stats;
    related_uids;
    root_directory = into.root_directory
  }

let merge_index ~store_shapes ~into index =
  let defs = merge index.defs into.defs in
  let approximated = merge index.approximated into.approximated in
  let stats = Stats.union (fun _ f1 _f2 -> Some f1) into.stats index.stats in
  let related_uids =
    Uid_map.union
      (fun _ a b -> Some (Union_find.union a b))
      index.related_uids into.related_uids
  in
  if store_shapes then
    Hashtbl.add_seq index.cu_shape (Hashtbl.to_seq into.cu_shape);
  { into with defs; approximated; stats; related_uids }

let from_files ~store_shapes ~output_file ~root ~rewrite_root ~build_path
    ~do_not_use_cmt_loadpath files =
  Log.debug "Debug log is enabled";
  let initial_index =
    { defs = Uid_map.empty ();
      approximated = Uid_map.empty ();
      cu_shape = Hashtbl.create 64;
      stats = Stats.empty;
      root_directory = root;
      related_uids = Uid_map.empty ()
    }
  in
  let final_index =
    Ocaml_utils.Local_store.with_store (Ocaml_utils.Local_store.fresh ())
    @@ fun () ->
    List.fold_left
      (fun into file ->
        match Cmt_cache.read file with
        | cmt_item ->
          index_of_cmt ~into ~root ~rewrite_root ~build_path ~store_shapes
            ~do_not_use_cmt_loadpath cmt_item.cmt_infos
        | exception _ -> (
          match read ~file with
          | Index index -> merge_index ~store_shapes ~into index
          | _ ->
            Log.error "Unknown file type: %s" file;
            exit 1))
      initial_index files
  in
  write ~file:output_file final_index
