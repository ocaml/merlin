type t

val make :
  ?for_completion:Msource.position -> Trace.t -> Mconfig.t -> Msource.t -> t

val input_config : t -> Mconfig.t
val input_source : t -> Msource.t

val with_reader : t -> (unit -> 'a) -> 'a
val reader_config : t -> Mconfig.t
val reader_comments : t -> (string * Location.t) list
val reader_parsetree : t -> Mreader.parsetree
val reader_lexer_errors : t -> exn list
val reader_parser_errors : t -> exn list
val reader_no_labels_for_completion : t -> bool

val ppx_parsetree : t -> Mreader.parsetree
val ppx_errors : t -> exn list

val final_config : t -> Mconfig.t

val typer_result : t -> Mtyper.result
val typer_errors : t -> exn list
