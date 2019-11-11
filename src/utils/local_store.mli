(* Dynamic-scoping for global piece of state *)

type bindings
val new_bindings : unit -> bindings

val ref : bindings -> (unit -> 'a) -> 'a ref

type scope
val fresh : bindings -> scope
val merge : scope -> scope -> scope
val with_scope : scope -> (unit -> 'a) -> 'a

(* Typechecker state *)

val typechecker_state : bindings

module Typechecker : sig
  (* Scopped-references *)
  val sref : (unit -> 'a) -> 'a ref
  val srefk : 'a -> 'a ref
end
