
(*val set_destination : Sturgeon_stub.cursor -> unit*)
(*val cursor : t -> Sturgeon_stub.cursor*)

type t

val null : t

val is_open : t -> bool
val is_closed : t -> bool

val start : ?limit:int -> unit -> t

val enter
  : t -> ('a, unit, string, (t -> 'b) -> 'b) format4 ->
  return:(unit -> 'b -> string) -> 'a

val message
  : t -> ('a, unit, string, unit) format4 -> 'a
