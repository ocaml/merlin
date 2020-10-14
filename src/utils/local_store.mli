(* Dynamic-scoping for global piece of state *)

val is_bound : unit -> bool
val reset : unit -> unit

val s_table : ('a -> 'b) -> 'a -> 'b ref
val s_ref : 'a -> 'a ref

type scope
val fresh : unit -> scope
val with_scope : scope -> (unit -> 'a) -> 'a
