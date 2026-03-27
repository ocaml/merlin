type 'a cell = {
  mutable content : 'a;
  mutable prev : 'a cell;
  mutable next : 'a cell;
}

type 'a t

val create : unit -> 'a t
val is_empty : 'a t -> bool
val add_front : 'a t -> 'a -> 'a cell
val discard : 'a t -> 'a
val promote : 'a t -> 'a cell -> unit
val get : 'a cell -> 'a
val promote_update : 'a t -> 'a cell -> 'a -> unit
