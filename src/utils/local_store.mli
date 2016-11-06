type bindings
val new_bindings : unit -> bindings

val ref : bindings -> (unit -> 'a) -> 'a ref

type scope
val fresh : bindings -> scope
val merge : scope -> scope -> scope
val with_scope : scope -> (unit -> 'a) -> 'a
