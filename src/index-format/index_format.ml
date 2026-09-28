exception Not_an_index of string

module Lid = Lid
module Lid_set = Granular_set.Make (Lid)
module Uid_map = Union_find.Uid_map
module Stats = Map.Make (String)
module Uid_set = Granular_set.Make (Shape.Uid)

module Union_find = struct
  type t = Uid_set.t Union_find.elt_handle Granular_marshal.link
  type store = Uid_set.t Union_find.content Uid_map.t

  let empty () = Union_find.empty ()

  let new_root store uid v =
    let store, root = Union_find.new_root store uid v in
    (store, Granular_marshal.link root)

  let get store t = Union_find.get store (Granular_marshal.fetch t)

  let union store a b =
    let open Granular_marshal in
    let store, _root =
      Union_find.union store ~f:Uid_set.union (fetch a) (fetch b)
    in
    (store, a)

  let merge = Union_find.merge ~f:Uid_set.union

  let merge_union store map store' map' =
    let store = ref (merge store store') in
    let map =
      Uid_map.union
        (fun _ a b ->
          let store', v = union !store a b in
          store := store';
          Some v)
        map map'
    in
    (!store, map)

  let type_id : t Type.Id.t = Type.Id.make ()

  let schema { Granular_marshal.yield } t =
    yield t type_id Granular_marshal.schema_no_sublinks
end

let add map uid locs =
  Uid_map.update uid
    (function
      | None -> Some locs
      | Some locs' -> Some (Lid_set.union locs' locs))
    map

type stat = { mtime : float; size : int; source_digest : string option }

type index =
  { defs : Lid_set.t Uid_map.t;
    approximated : Lid_set.t Uid_map.t;
    cu_shape : (string, Shape.t) Hashtbl.t;
    stats : stat Stats.t;
    root_directory : string option;
    related_uids_store : Union_find.store;
    related_uids : Union_find.t Uid_map.t
  }

let lidset_schema iter lidset = Lid_set.schema iter Lid.schema lidset
let uidset_schema iter lidset =
  Uid_set.schema iter Granular_marshal.schema_no_sublinks lidset

let type_setmap : Lid_set.t Uid_map.t Type.Id.t = Type.Id.make ()
let type_ufmap : Union_find.t Uid_map.t Type.Id.t = Type.Id.make ()
let type_ufstore : Union_find.store Type.Id.t = Type.Id.make ()

let index_schema (iter : Granular_marshal.iter) index =
  Uid_map.schema type_setmap iter
    (fun iter _ v -> lidset_schema iter v)
    index.defs;
  Uid_map.schema type_setmap iter
    (fun iter _ v -> lidset_schema iter v)
    index.approximated;
  Uid_map.schema type_ufmap iter
    (fun iter _ v -> Union_find.schema iter v)
    index.related_uids;
  Uid_map.schema type_ufstore iter
    (fun iter _uid -> function
      | Link _ -> ()
      | Root { value; _ } -> uidset_schema iter value)
    index.related_uids_store

let compress index =
  let cache = Lid.cache () in
  let compress_map_set =
    Uid_map.iter_in_memory (fun _ ->
        Lid_set.iter_in_memory (Lid.deduplicate cache))
  in
  compress_map_set index.defs;
  compress_map_set index.approximated;
  let related_uids =
    (* Uid_map.map
      (fun set ->
        let uid = Uid_set.min_elt (Union_find.get set) in
        let reference_set = Uid_map.find uid index.related_uids in
        Granular_marshal.reuse reference_set;
        reference_set) *)
    index.related_uids
  in
  { index with related_uids }

let pp_lidset fmt locs =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@;")
    Lid.pp fmt (Lid_set.elements locs)

let pp_partials (fmt : Format.formatter) (partials : Lid_set.t Uid_map.t) =
  Format.fprintf fmt "{@[";
  Uid_map.iter
    (fun uid locs ->
      Format.fprintf fmt "@[<hov 2>uid: %a; locs:@ @[<v>%a@]@]@;"
        Shape.Uid.print uid pp_lidset locs)
    partials;
  Format.fprintf fmt "@]}"

let pp_related_uids (related_uids_store : Union_find.store)
    (fmt : Format.formatter) (related_uids : Union_find.t Uid_map.t) =
  let rec gather acc map =
    match Uid_map.choose_opt map with
    | Some (_key, union) ->
      let group = Union_find.get related_uids_store union |> Uid_set.elements in
      List.fold_left (fun acc key -> Uid_map.remove key acc) map group
      |> gather (group :: acc)
    | None -> acc
  in
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@;")
    (fun fmt group ->
      Format.fprintf fmt "(%a)"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
           Shape.Uid.print)
        group)
    fmt (gather [] related_uids)

let pp (fmt : Format.formatter) pl =
  Format.fprintf fmt "%i uids:@ {@[" (Uid_map.cardinal pl.defs);
  Uid_map.iter
    (fun uid locs ->
      Format.fprintf fmt "@[<hov 2>uid: %a; locs:@ @[<v>%a@]@]@;"
        Shape.Uid.print uid pp_lidset locs)
    pl.defs;
  Format.fprintf fmt "@]},@ ";
  Format.fprintf fmt "%i approx shapes:@ @[%a@],@ "
    (Uid_map.cardinal pl.approximated)
    pp_partials pl.approximated;
  Format.fprintf fmt "and shapes for CUS %s.@ "
    (String.concat ";@," (Hashtbl.to_seq_keys pl.cu_shape |> List.of_seq));
  Format.fprintf fmt "and related uids:@[{%a}@]"
    (pp_related_uids pl.related_uids_store)
    pl.related_uids

let ext = "ocaml-index"

let magic_number = Config.index_magic_number

let write ~file index =
  let index = compress index in
  Misc.output_to_file_via_temporary ~mode:[ Open_binary ] file
    (fun _temp_file_name oc ->
      output_string oc magic_number;
      let id = Random.State.(full_int (make_self_init ()) max_int) in
      Granular_marshal.write oc ~filename:file ~id index_schema (index : index))

type file_content = Cmt of Cmt_format.cmt_infos | Index of index | Unknown

let read ~file =
  let ic = open_in_bin file in
  Ocaml_utils.Misc.try_finally
    ~always:(fun () -> close_in ic)
    (fun () ->
      let file_magic_number = ref (Cmt_format.read_magic_number ic) in
      let cmi_magic_number = Ocaml_utils.Config.cmi_magic_number in
      let cmt_magic_number = Ocaml_utils.Config.cmt_magic_number in
      (if String.equal !file_magic_number cmi_magic_number then
         let _ = Cmi_format.input_cmi ic in
         file_magic_number := Cmt_format.read_magic_number ic);
      if String.equal !file_magic_number cmt_magic_number then
        Cmt (input_value ic : Cmt_format.cmt_infos)
      else if String.equal !file_magic_number magic_number then
        Index (Granular_marshal.read file ic index_schema)
      else Unknown)

let read_exn ~file =
  match read ~file with
  | Index index -> index
  | _ -> raise (Not_an_index file)
