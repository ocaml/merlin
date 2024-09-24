exception Not_an_index of string

module Lid : Set.OrderedType with type t = Longident.t Location.loc = struct
  type t = Longident.t Location.loc

  let compare_pos (p1 : Lexing.position) (p2 : Lexing.position) =
    let p1f, p2f = Filename.(basename p1.pos_fname, basename p2.pos_fname) in
    match String.compare p1f p2f with
    | 0 -> Int.compare p1.pos_cnum p2.pos_cnum
    | n -> n

  let compare (t1 : t) (t2 : t) =
    match compare_pos t1.loc.loc_start t2.loc.loc_start with
    | 0 -> compare_pos t1.loc.loc_end t2.loc.loc_end
    | n -> n
end

module Lid_set = Set.Make (Lid)
module Uid_map = Shape.Uid.Map
module Stats = Map.Make (String)

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
    root_directory : string option
  }

let pp_partials (fmt : Format.formatter) (partials : Lid_set.t Uid_map.t) =
  Format.fprintf fmt "{@[";
  Uid_map.iter
    (fun uid locs ->
      Format.fprintf fmt "@[<hov 2>uid: %a; locs:@ @[<v>%a@]@]@;"
        Shape.Uid.print uid
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@;")
           (fun fmt { Location.txt; loc } ->
             Format.fprintf fmt "%S: %a"
               (try Longident.flatten txt |> String.concat "." with _ -> "<?>")
               Location.print_loc loc))
        (Lid_set.elements locs))
    partials;
  Format.fprintf fmt "@]}"

let pp (fmt : Format.formatter) pl =
  Format.fprintf fmt "%i uids:@ {@[" (Uid_map.cardinal pl.defs);
  Uid_map.iter
    (fun uid locs ->
      Format.fprintf fmt "@[<hov 2>uid: %a; locs:@ @[<v>%a@]@]@;"
        Shape.Uid.print uid
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@;")
           (fun fmt { Location.txt; loc } ->
             Format.fprintf fmt "%S: %a"
               (try Longident.flatten txt |> String.concat "." with _ -> "<?>")
               Location.print_loc loc))
        (Lid_set.elements locs))
    pl.defs;
  Format.fprintf fmt "@]},@ ";
  Format.fprintf fmt "%i approx shapes:@ @[%a@],@ "
    (Uid_map.cardinal pl.approximated)
    pp_partials pl.approximated;
  Format.fprintf fmt "and shapes for CUS %s.@ "
    (String.concat ";@," (Hashtbl.to_seq_keys pl.cu_shape |> List.of_seq))

let ext = "ocaml-index"

let magic_number = Config.index_magic_number

let write ~file index =
  Misc.output_to_file_via_temporary ~mode:[ Open_binary ] file
    (fun _temp_file_name oc ->
      output_string oc magic_number;
      output_value oc (index : index))

type file_content = Cmt of Cmt_format.cmt_infos | Index of index | Unknown

let read ~file =
  let ic = open_in_bin file in
  Merlin_utils.Misc.try_finally
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
        Index (input_value ic : index)
      else Unknown)

let read_exn ~file =
  match read ~file with
  | Index index -> index
  | _ -> raise (Not_an_index file)
