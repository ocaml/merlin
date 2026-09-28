exception Not_an_index of string

val ext : string
val magic_number : string

module Lid : sig
  include Set.OrderedType
  val of_lid : Longident.t Location.loc -> t
  val to_lid : t -> Longident.t Location.loc
end
module Lid_set : Granular_set.S with type elt = Lid.t
module Stats : Map.S with type key = String.t
module Uid_set : Granular_set.S with type elt = Shape.Uid.t
module Uid_map : Granular_map.S with type key = Shape.Uid.t
module Union_find : sig
  type t
  type store = Uid_set.t Union_find.content Uid_map.t
  val empty : unit -> store

  val new_root : store -> Shape.Uid.t -> Uid_set.t -> store * t
  val get : store -> t -> Uid_set.t
  val union : store -> t -> t -> store * t

  val merge : store -> store -> store

  (** [merge_union store map store' map'] combines two union-find structures,
      each described by a [store] (mapping uids to their union-find content) and
      a [map] (mapping uids to handles into that store). *)
  val merge_union :
    store -> t Uid_map.t -> store -> t Uid_map.t -> store * t Uid_map.t
end

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

val pp : Format.formatter -> index -> unit

(** [add tbl uid locs] adds a binding of [uid] to the locations [locs]. If this
    key is already present the locations are merged. *)
val add : Lid_set.t Uid_map.t -> Shape.Uid.t -> Lid_set.t -> Lid_set.t Uid_map.t

type file_content = Cmt of Cmt_format.cmt_infos | Index of index | Unknown

val write : file:string -> index -> unit
val read : file:string -> file_content

val read_exn : file:string -> index
