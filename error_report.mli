(** {0 Mise en forme d'exceptions pour le reporting} *)
exception Warning of Location.t * string
val reset_warnings : unit -> exn list

(** Le format d'une exception reconnue est :
  * {message: string, type: string, valid:true,
  *  start: { line: int, col : int },   // position
  *  end: { line: int, col : int },
  *  type: "type"/"parser"/"unknown",  // type d'erreur
  * }
  * Une exception non reconnue :
  * {message: ''Printexc.to_string'', valid:false}
  *)
val to_json : exn -> Json.json option
val to_jsons : exn list -> Json.json list
