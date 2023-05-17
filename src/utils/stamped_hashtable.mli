type ('a, 'b) t
type changes

val create : changes -> int -> ('a, 'b) t
val add : ('a, 'b) t -> ?stamp:int -> 'a -> 'b -> unit
val mem : ('a, 'b) t -> 'a -> bool
val find : ('a, 'b) t -> 'a -> 'b

val create_changes : unit -> changes

(* [backtrack changes ~stamp] remove all items added to tables created using
   [changes] with a stamp strictly greater than [stamp] *)
val backtrack : changes -> stamp:int -> unit
