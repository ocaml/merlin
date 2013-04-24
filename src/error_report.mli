(** {0 Exception formatting for error reporting} *)

(** The format of reports for known exceptions is:
  *   {message: string, type: string, valid:true,
  *    start: { line: int, col : int },   // position
  *    end: { line: int, col : int },
  *    type: "type"/"parser"/"unknown",  // error type
  *   }
  * Unknown exceptions use:
  *   {message: ''Printexc.to_string'', valid:false}
  *)

(* Ignore unknown exceptions *)
val strict_to_json : exn -> (Location.t * Json.json) option
val strict_to_jsons : exn list -> (Location.t * Json.json) list

(* Generic handling of unknown exceptions *)
val to_json : exn -> Location.t * Json.json
val to_jsons : exn list -> (Location.t * Json.json) list
