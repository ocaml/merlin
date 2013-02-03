type state = {
  pos      : Lexing.position;
  tokens   : Outline.token list;
  outlines : Outline.t;
  chunks   : Chunk.t;
  types    : Typer.t;
}
val initial_state : state 

type handler = Protocol.io -> state -> Json.json list -> state * Json.json
type t = { name : string ; handler : handler ; doc : string }
val invalid_arguments : unit -> 'a

val commands : (string,t) Hashtbl.t
val register : t -> unit

val source_path : string list ref
val reset_global_modules : unit -> unit
