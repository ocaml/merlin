type t

(* Except inside Mpipeline, this function should only be used in old_merlin *)
val make :
  ?position:int * int -> Mconfig.t -> Msource.t -> t Domain_msg.t -> t option

(* Except inside Mpipeline, this function should only be used in old_merlin *)
val with_pipeline : t -> (unit -> 'a) -> 'a

(* val for_completion : Msource.position -> t -> t *)

val raw_source : t -> Msource.t

val input_config : t -> Mconfig.t
val input_source : t -> Msource.t
val get_lexing_pos : t -> [< Msource.position ] -> Lexing.position

val reader_config : t -> Mconfig.t
val reader_comments : t -> (string * Location.t) list
val reader_parsetree : t -> Mreader.parsetree
val reader_lexer_keywords : t -> string list
val reader_lexer_errors : t -> exn list
val reader_parser_errors : t -> exn list
val reader_no_labels_for_completion : t -> bool

val ppx_parsetree : t -> Mreader.parsetree
val ppx_errors : t -> exn list

val final_config : t -> Mconfig.t

val typer_result : t -> Mtyper.result
val typer_errors : t -> exn list

val timing_information : t -> (string * float) list
val cache_information : t -> Std.json

module Cache : sig
  val get : Mconfig.t -> Mocaml.typer_state
end

val close_typer : t Domain_msg.t -> unit
val cancel_typer : t Domain_msg.t -> unit
val share_exn : t Domain_msg.t -> exn -> unit

val domain_typer : t Domain_msg.t -> unit -> unit
val get : ?position:int * int -> t Domain_msg.t -> Mconfig.t -> Msource.t -> t
