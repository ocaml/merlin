type t =
  { occurrences : Query_protocol.occurrence list;
    status : Query_protocol.occurrences_status
  }

(** Find occurrences of an unparsed identifier (longident) *)
val locs_of :
  config:Mconfig.t ->
  env:Env.t ->
  typer_result:Mtyper.result ->
  pos:Lexing.position ->
  scope:[ `Project | `Buffer | `Renaming ] ->
  string ->
  t
