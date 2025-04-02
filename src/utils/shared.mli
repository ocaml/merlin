type 'a t = { mutex : Mutex.t; cond : Condition.t; mutable value : 'a }
val locking_set : 'a t -> 'a -> unit
val set : 'a t -> 'a -> unit
val locking_get : 'a t -> 'a
val get : 'a t -> 'a
val create : 'a -> 'a t
val protect : 'a t -> (unit -> 'b) -> 'b
val signal : 'a t -> unit
val wait : 'a t -> unit
val lock : 'a t -> unit
val unlock : 'a t -> unit
