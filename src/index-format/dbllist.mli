type 'a cell =
  { mutable content : 'a;
    weight : int;
    mutable prev : 'a cell;
    mutable next : 'a cell
  }

type 'a t

(* val clear : 'a t -> unit *)
val create : int -> 'a t
(* val is_empty : 'a t -> bool *)
val add_front : 'a t -> 'a * int -> 'a cell
val discard : 'a t -> 'a
val discard_size : 'a t -> int -> 'a list
(* val discard_cell : 'a t -> 'a cell -> 'a *)
val promote : 'a t -> 'a cell -> unit
val get : 'a cell -> 'a
val promote_update : 'a t -> 'a cell -> 'a -> unit
val pp_stats : 'a t -> unit