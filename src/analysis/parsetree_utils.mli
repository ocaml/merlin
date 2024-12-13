(** Utilities to provide a slightly more stable Parsetree API for alternative
  clients like [ocaml-lsp]. *)

open Parsetree

type nonrec constant_desc = constant_desc

val constant_desc : constant -> constant_desc
