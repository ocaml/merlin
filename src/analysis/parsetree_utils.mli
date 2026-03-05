(** Utilities to provide a slightly more stable Parsetree API for alternative
  clients like [ocaml-lsp]. *)

open Parsetree

type nonrec constant_desc = constant_desc

val constant_desc : constant -> constant_desc

(** Filter parsetree attributes which are prefixed by ["merlin."] in given expression. *)
val expr_remove_merlin_attributes : expression -> expression
