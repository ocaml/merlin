(** {0 Exception formatting for error reporting} *)
exception Warning of Location.t * string
val reset_warnings : unit -> exn list

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
val strict_to_json : exn -> Json.json option
val strict_to_jsons : exn list -> Json.json list

(* Generic handling of unknown exceptions *)
val to_json : exn -> Json.json
val to_jsons : exn list -> Json.json list
