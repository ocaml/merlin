type t

val make :
  Trace.t -> Mconfig.t -> Msource.t -> t

val make_for_completion :
  Trace.t -> Mconfig.t -> Msource.t -> Msource.position ->
  [`No_labels of bool] * t

val release : t -> unit

val dump : t -> Std.json

(* Accessors *)

type parsetree = [
  | `Interface of Parsetree.signature
  | `Implementation of Parsetree.structure
]

val get_parsetree : t -> parsetree
val get_errors : t -> Location.error list
val get_config : t -> Mconfig.t

(* Pretty-printing *)

type pretty_parsetree = Extend_protocol.Reader.pretty_parsetree
type outcometree = Extend_protocol.Reader.outcometree

val print_pretty : t -> pretty_parsetree -> string
val print_outcome : t -> outcometree -> string
val print_batch_outcome : t -> outcometree list -> string list
