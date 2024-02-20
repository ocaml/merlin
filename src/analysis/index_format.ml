module Lid : Set.OrderedType with type t = Longident.t Location.loc = struct
  type t = Longident.t Location.loc

  let compare_pos (p1 : Lexing.position) (p2 : Lexing.position) =
    match String.compare p1.pos_fname p2.pos_fname with
    | 0 -> Int.compare p1.pos_cnum p2.pos_cnum
    | n -> n

  let compare (t1 : t) (t2 : t) =
    (* TODO CHECK...*)
    match compare_pos t1.loc.loc_start t2.loc.loc_start with
    | 0 -> compare_pos t1.loc.loc_end t2.loc.loc_end
    | n -> n
end

module LidSet = Set.Make (Lid)

(** [add tbl uid locs] adds a binding of [uid] to the locations [locs]. If this key is
    already present the locations are merged. *)
let add tbl uid locs =
  try
    let locations = Hashtbl.find tbl uid in
    Hashtbl.replace tbl uid (LidSet.union locs locations)
  with Not_found -> Hashtbl.add tbl uid locs

type payload = {
  defs : (Shape.Uid.t, LidSet.t) Hashtbl.t;
  partials : (Shape.t, LidSet.t) Hashtbl.t;
  unreduced : (Shape.t * Longident.t Location.loc) list;
  load_path : string list;
  cu_shape : (string, Shape.t) Hashtbl.t;
}

type file_format = V1 of payload

let pp_partials (fmt : Format.formatter)
    (partials : (Shape.t, LidSet.t) Hashtbl.t) =
  Format.fprintf fmt "{@[";
  Hashtbl.iter
    (fun shape locs ->
      Format.fprintf fmt "@[<hov 2>shape: %a; locs:@ @[<v>%a@]@]@;" Shape.print
        shape
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@;")
           (fun fmt { Location.txt; loc } ->
             Format.fprintf fmt "%S: %a"
               (try Longident.flatten txt |> String.concat "." with _ -> "<?>")
               Location.print_loc loc))
        (LidSet.elements locs))
    partials;
  Format.fprintf fmt "@]}"

let pp_unreduced (fmt : Format.formatter)
    (unreduced : (Shape.t * Longident.t Location.loc) list) =
  Format.fprintf fmt "{@[";
  List.iter
    (fun (shape, { Location.txt; loc }) ->
      Format.fprintf fmt "@[<hov 2>shape: %a; locs:@ @[<v>%s: %a@]@]@;"
        Shape.print shape
        (try Longident.flatten txt |> String.concat "." with _ -> "<?>")
        Location.print_loc loc)
    unreduced;
  Format.fprintf fmt "@]}"

let pp_payload (fmt : Format.formatter) pl =
  Format.fprintf fmt "%i uids:@ {@[" (Hashtbl.length pl.defs);
  Hashtbl.iter
    (fun uid locs ->
      Format.fprintf fmt "@[<hov 2>uid: %a; locs:@ @[<v>%a@]@]@;"
        Shape.Uid.print uid
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@;")
           (fun fmt { Location.txt; loc } ->
             Format.fprintf fmt "%S: %a"
               (try Longident.flatten txt |> String.concat "." with _ -> "<?>")
               Location.print_loc loc))
        (LidSet.elements locs))
    pl.defs;
  Format.fprintf fmt "@]},@ ";
  Format.fprintf fmt "%i partial shapes:@ @[%a@],@ "
    (Hashtbl.length pl.partials)
    pp_partials pl.partials;
  Format.fprintf fmt "%i unreduced shapes:@ @[%a@]@ " (List.length pl.unreduced)
    pp_unreduced pl.unreduced;
  Format.fprintf fmt "and shapes for CUS %s.@ "
    (String.concat ";@," (Hashtbl.to_seq_keys pl.cu_shape |> List.of_seq))

let pp (fmt : Format.formatter) ff =
  match ff with V1 tbl -> Format.fprintf fmt "V1@,%a" pp_payload tbl

let ext = "uideps"

let write ~file tbl =
  let oc = open_out_bin file in
  Marshal.to_channel oc (V1 tbl) [];
  close_out oc

let read ~file =
  let ic = open_in_bin file in
  try
    let payload =
      match Marshal.from_channel ic with V1 payload -> payload
      (* TODO is that "safe" ? We probably want some magic number *)
    in
    close_in ic;
    payload
  with e -> raise e (* todo *)
