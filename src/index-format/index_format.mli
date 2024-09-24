exception Not_an_index of string

val ext : string
val magic_number : string

module Lid : Set.OrderedType with type t = Longident.t Location.loc
module Lid_set : Set.S with type elt = Lid.t
module Stats : Map.S with type key = String.t
module Uid_map = Shape.Uid.Map

type stat = { mtime : float; size : int; source_digest : string option }

type index =
  { defs : Lid_set.t Uid_map.t;
    approximated : Lid_set.t Uid_map.t;
    cu_shape : (string, Shape.t) Hashtbl.t;
    stats : stat Stats.t;
    root_directory : string option
  }

val pp : Format.formatter -> index -> unit

(** [add tbl uid locs] adds a binding of [uid] to the locations [locs]. If this
    key is already present the locations are merged. *)
val add : Lid_set.t Uid_map.t -> Shape.Uid.t -> Lid_set.t -> Lid_set.t Uid_map.t

type file_content = Cmt of Cmt_format.cmt_infos | Index of index | Unknown

val write : file:string -> index -> unit
val read : file:string -> file_content
val read_exn : file:string -> index
