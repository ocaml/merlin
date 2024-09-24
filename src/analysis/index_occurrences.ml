open Std
module Lid_set = Index_format.Lid_set
let { Logger.log } = Logger.for_section "index-occurrences"

let set_fname ~file (loc : Location.t) =
  let pos_fname = file in
  { loc with
    loc_start = { loc.loc_start with pos_fname };
    loc_end = { loc.loc_end with pos_fname }
  }

let decl_of_path_or_lid env namespace path lid =
  match (namespace : Shape.Sig_component_kind.t) with
  | Constructor -> begin
    match Env.find_constructor_by_name lid env with
    | exception Not_found -> None
    | { cstr_uid; cstr_loc; _ } ->
      Some { Env_lookup.uid = cstr_uid; loc = cstr_loc; namespace }
  end
  | Label -> begin
    match Env.find_label_by_name lid env with
    | exception Not_found -> None
    | { lbl_uid; lbl_loc; _ } ->
      Some { Env_lookup.uid = lbl_uid; loc = lbl_loc; namespace }
  end
  | _ -> Env_lookup.by_path path namespace env

let iterator ~current_buffer_path ~index ~stamp ~reduce_for_uid =
  let add uid loc = Stamped_hashtable.add index ~stamp (uid, loc) () in
  let f ~namespace env path (lid : Longident.t Location.loc) =
    log ~title:"index_buffer" "Path: %a" Logger.fmt (Fun.flip Path.print path);
    let not_ghost { Location.loc = { loc_ghost; _ }; _ } = not loc_ghost in
    let lid = { lid with loc = set_fname ~file:current_buffer_path lid.loc } in
    let index_decl () =
      begin
        match decl_of_path_or_lid env namespace path lid.txt with
        | (exception _) | None ->
          log ~title:"index_buffer" "Declaration not found"
        | Some decl ->
          log ~title:"index_buffer" "Found declaration: %a" Logger.fmt
            (Fun.flip Location.print_loc decl.loc);
          add decl.uid lid
      end
    in
    if not_ghost lid then
      match Env.shape_of_path ~namespace env path with
      | exception Not_found -> ()
      | path_shape ->
        log ~title:"index_buffer" "Shape of path: %a" Logger.fmt
          (Fun.flip Shape.print path_shape);
        let result = reduce_for_uid env path_shape in
        begin
          match Locate.uid_of_result ~traverse_aliases:false result with
          | Some uid, false ->
            log ~title:"index_buffer" "Found %a (%a) wiht uid %a" Logger.fmt
              (Fun.flip Pprintast.longident lid.txt)
              Logger.fmt
              (Fun.flip Location.print_loc lid.loc)
              Logger.fmt
              (Fun.flip Shape.Uid.print uid);
            add uid lid
          | Some uid, true ->
            log ~title:"index_buffer" "Shape is approximative, found uid: %a"
              Logger.fmt
              (Fun.flip Shape.Uid.print uid);
            index_decl ()
          | None, _ ->
            log ~title:"index_buffer" "Reduction failed: missing uid";
            index_decl ()
        end
  in
  Ast_iterators.iterator_on_usages ~f

let items ~index ~stamp (config : Mconfig.t) items =
  let module Shape_reduce = Shape_reduce.Make (struct
    let fuel = 10

    let read_unit_shape ~unit_name =
      log ~title:"read_unit_shape" "inspecting %s" unit_name;
      let cmt = Format.sprintf "%s.cmt" unit_name in
      match Cmt_cache.read (Load_path.find_normalized cmt) with
      | { cmt_infos = { cmt_impl_shape; _ }; _ } ->
        log ~title:"read_unit_shape" "shapes loaded for %s" unit_name;
        cmt_impl_shape
      | exception _ ->
        log ~title:"read_unit_shape" "failed to find %s" unit_name;
        None
  end) in
  let current_buffer_path =
    Filename.concat config.query.directory config.query.filename
  in
  let reduce_for_uid = Shape_reduce.reduce_for_uid in
  let iterator = iterator ~current_buffer_path ~index ~stamp ~reduce_for_uid in
  match items with
  | `Impl items -> List.iter ~f:(iterator.structure_item iterator) items
  | `Intf items -> List.iter ~f:(iterator.signature_item iterator) items
