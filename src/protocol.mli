type io = Json.json Stream.t * (Json.json -> unit)

val make : input:in_channel -> output:out_channel -> io
val log  : dest:out_channel -> io -> io

val return : Json.json -> Json.json
val fail   : exn -> Json.json

(* HACK. Break circular reference:
 * Error_report uses Protocol to format error positions.
 * Protocol uses Error_report to format standard errors.
 *)
val error_catcher : (exn -> (Location.t * Json.json) option) ref

val make_pos : int * int -> Lexing.position
val pos_to_json : Lexing.position -> Json.json
val pos_of_json : Json.json -> Lexing.position
val with_location : Location.t -> (string * Json.json) list -> Json.json
