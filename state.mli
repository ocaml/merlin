type t = {
  pos      : Lexing.position;
  tokens   : Outline.token list;
  outlines : Outline.t;
  chunks   : Chunk.t;
  types    : Typer.t;
}
val initial : t

val source_path : string list ref
val reset_global_modules : unit -> unit

val node_at : t -> Lexing.position -> Browse.t
val node_complete : Browse.t -> string -> Json.json list

val exceptions : t -> exn list
