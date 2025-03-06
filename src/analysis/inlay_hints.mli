(** Builds the list of inlay hints to be displayed on a document. *)

type hint = Lexing.position * string

val of_structure :
  hint_let_binding:bool ->
  hint_pattern_binding:bool ->
  start:Lexing.position ->
  stop:Lexing.position ->
  Typedtree.structure ->
  hint list
