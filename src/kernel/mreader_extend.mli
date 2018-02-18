type t

val stop : Trace.t -> t -> unit

val start : Trace.t -> string -> string list -> Mconfig.t -> Msource.t -> t option

val parse : Trace.t -> ?for_completion:Msource.position ->
  t ->
  ([`No_labels of bool ] *
   [`Implementation of Parsetree.structure | `Interface of Parsetree.signature])
  option

val reconstruct_identifier :
  Trace.t -> Lexing.position -> t -> string Location.loc list option

val print_pretty :
  Trace.t -> Extend_protocol.Reader.pretty_parsetree -> t -> string option

val print_outcomes :
  Trace.t -> Extend_protocol.Reader.outcometree list -> t -> string list option

val print_outcome :
  Trace.t -> Extend_protocol.Reader.outcometree -> t -> string option

val print : unit -> t -> string
