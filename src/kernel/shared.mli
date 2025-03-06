type 'a t = { mutex : Mutex.t; cond : Condition.t; mutable value : 'a }
val set : 'a t -> 'a -> unit
val locking_set : 'a t -> 'a -> unit
val get : 'a t -> 'a
val locking_get : 'a t -> 'a
val create : 'a -> 'a t
val signal : 'a t -> unit
val wait : 'a t -> unit
val protect : 'a t -> (unit -> 'b) -> 'b
