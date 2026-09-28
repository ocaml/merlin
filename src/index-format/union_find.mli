module Uid = Shape.Uid

module Uid_map : Granular_map.S with type key = Uid.t

type 'a elt_handle = Uid.t
type 'a content = Root of { value : 'a; rank : int } | Link of 'a elt_handle
type 'a store = 'a content Uid_map.t
val empty : unit -> 'a store
val new_root : 'a store -> Uid.t -> 'a -> 'a store * 'a elt_handle
val get : 'a store -> 'a elt_handle -> 'a
val union :
  f:('a -> 'a -> 'a) ->
  'a store ->
  'a elt_handle ->
  'a elt_handle ->
  'a store * 'a elt_handle

(** [f] must be idempotent, commutative and associative. *)
val merge : f:('a -> 'a -> 'a) -> 'a store -> 'a store -> 'a store
