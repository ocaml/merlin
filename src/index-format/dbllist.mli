type 'a cell =
  { content : 'a; weight : int; mutable prev : 'a cell; mutable next : 'a cell }

type stats =
  { mutable total_cap : int;
    mutable promote_count : int;
    mutable add_count : int;
    mutable discard_count : int;
    mutable add_size : int;
    mutable discarded_size : int
  }

type 'a dbll =
  | Nil of int
  | List of { first : 'a cell; last : 'a cell; size : int; cap : int }

type 'a t = { mutable dbll : 'a dbll; stats : stats }

exception Action_on_empty_list of string

val pp_stats : 'a t -> unit
val create : int -> 'a t
val add_front : 'a t -> 'a * int -> 'a cell
val discard_size : 'a t -> int -> 'a list
val promote : 'a t -> 'a cell -> unit
val get : 'a cell -> 'a
