
val set_destination : Sturgeon_stub.cursor -> unit

type t

val cursor : t -> Sturgeon_stub.cursor
val is_open : t -> bool
val is_closed : t -> bool

val start : ?limit:int -> unit -> t

val enter
  : t -> ('a, unit, string, (t -> 'b) -> 'b) format4 -> return:(Format.formatter -> 'b -> unit) -> 'a

val step
  : t -> ('a, unit, string, (t -> 'b) -> 'b) format4 -> return:(Format.formatter -> 'b -> unit) -> 'a
