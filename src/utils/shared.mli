type 'a t

val create : unit -> 'a t

val put_ack : 'a t -> 'a -> unit
val take : 'a t -> 'a
val unsafe_get : 'a t -> 'a option
val protect : 'a t -> (unit -> 'b) -> 'b
val signal : 'a t -> unit
val wait : 'a t -> unit
val lock : 'a t -> unit
val unlock : 'a t -> unit
