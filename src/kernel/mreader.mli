type parsetree = [
  | `Interface of Parsetree.signature
  | `Implementation of Parsetree.structure
]

type comment = (string * Location.t)

type result = {
  config        : Mconfig.t;
  lexer_errors  : exn list;
  parser_errors : exn list;
  comments      : comment list;
  parsetree     : parsetree;
  no_labels_for_completion : bool;
}

(* Entry points *)

val run :
  ?for_completion:Msource.position ->
  Trace.t -> Mconfig.t -> Msource.t -> result

(* Pretty-printing *)

type pretty_parsetree = Extend_protocol.Reader.pretty_parsetree

type outcometree = Extend_protocol.Reader.outcometree

val print_pretty : Mconfig.t -> pretty_parsetree -> string
val print_outcome : Mconfig.t -> outcometree -> string
val print_batch_outcome : Mconfig.t -> outcometree list -> string list
