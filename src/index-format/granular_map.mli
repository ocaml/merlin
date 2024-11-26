open Granular_marshal

module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type key
  type 'a s =
  | Empty
  | Node of {l:'a t; v:key; d:'a; r:'a t; h:int}
  and 'a t = 'a s link

  val empty: unit -> 'a t
  val bindings: 'a t -> (key * 'a) list
  val add: key -> 'a -> 'a t -> 'a t
  val singleton: key -> 'a -> 'a t
  val remove: key -> 'a t -> 'a t
  val union: (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val cardinal: 'a t -> int
  val find: key -> 'a t -> 'a
  val find_opt: key -> 'a t -> 'a option
  val iter: (key -> 'a -> unit) -> 'a t -> unit
  val fold: (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
  val map: ('a -> 'b) -> 'a t -> 'b t
  val is_empty: 'a t -> bool
  val mem: key -> 'a t -> bool
  val update: key -> ('a option -> 'a option) -> 'a t -> 'a t
  val schema: Granular_marshal.iter ->
    (key -> 'a -> unit) -> 'a s Granular_marshal.link -> unit
end

module Make (Ord : OrderedType) : S with type key = Ord.t

