type io = Json.json Stream.t * (Json.json -> unit)

val make : input:in_channel -> output:out_channel -> io
val log  : dest:out_channel -> io -> io

val return : Json.json -> Json.json
val fail   : exn -> Json.json

val pos_to_json : Lexing.position -> Json.json
val pos_of_json : Json.json -> int * int
