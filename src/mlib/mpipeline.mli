type t

val make : Trace.t -> Mconfig.t -> Msource.t -> t

val input_config : t -> Mconfig.t
val input_source : t -> Msource.t

val reader_config : t -> Mconfig.t
val reader_comments : t -> (string * Location.t) list
val reader_parsetree : t -> Mreader.parsetree
val reader_lexer_errors : t -> exn list
val reader_parser_errors : t -> exn list

val ppx_parsetree : t -> Mreader.parsetree
val ppx_config : t -> Mconfig.t
val ppx_errors : t -> exn list

val typer_result : t -> Mtyper.result
val typer_errors : t -> exn list
