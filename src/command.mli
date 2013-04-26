type handler = Protocol.io -> State.t -> Json.json list -> State.t * Json.json
type t = { name : string ; handler : handler }
val invalid_arguments : unit -> 'a

val commands : (string,t) Hashtbl.t
val register : t -> unit

val load_packages : string list -> unit
