type ('a, 'b) t

type changelog

val create : changelog -> int -> ('a, 'b) t

val add : ('a, 'b) t -> ?stamp:int -> 'a -> 'b -> unit
val mem : ('a, 'b) t -> 'a -> bool
val find : ('a, 'b) t -> 'a -> 'b

val create_changelog : unit -> changelog

(* [backtrack changelog ~stamp] remove all items added to tables logging to
   [changelog] with a stamp strictly greater than [stamp] *)
val backtrack : changelog -> stamp:int -> unit
