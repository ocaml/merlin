type ('a, 'b) t
type changes

val create : int -> ('a, 'b) t
val add : ('a, 'b) t -> stamp:int -> 'a -> 'b -> unit
val mem : ('a, 'b) t -> 'a -> bool
val find : ('a, 'b) t -> 'a -> 'b

(* [backtrack table ~stamp] remove all items of [table] with a stamp strictly
   greater than [stamp] *)
val backtrack : ('a, 'b) t -> stamp:int -> unit
