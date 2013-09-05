type position = Lexing.position

type completion = { 
  name: string;
  kind: [`Value|`Constructor|`Label|
         `Module|`Modtype|`Type|`MethodCall];
  desc: string;
  info: string;
}

type _ request =
  | Tell 
    :  [`Source of string | `More of string | `End]
    -> bool request
  | Type_expr
    :  string * position option
    -> string request
  | Type_at
    :  position
    -> (Location.t * string) request
  | Type_enclosing
    :  position 
    -> (int * (Location.t * string) list) request
  | Complete_prefix 
    :  string * position option 
    -> completion list request
  | Locate          
    :  string * position option 
    -> (string * position) option request
  | Drop 
    :  position request
  | Seek 
    :  [`Position|`End|`Maximize_scope|`Before of position|`Exact of position]
    -> position request
  | Boundary 
    :  [`Prev|`Next|`Current] * position option 
    -> Location.t option request
  | Reset
    :  string option 
    -> unit request
  | Refresh 
    :  [`Full|`Quick]
    -> bool request
  | Cd 
    :  string 
    -> unit request
  | Errors 
    :  exn list request 
  | Dump 
    :  [`Env of position option|`Sig|`Chunks|`Tree|`Outline|`Exn]
    -> Json.json request
  | Which_path
    :  string
    -> string request
  | Which_with_ext
    :  string
    -> string list request
  | Findlib_use
    :  string list 
    -> unit request
  | Findlib_list
    :  string list request
  | Extension_list
    :  [`All|`Enabled|`Disabled]
    -> string list request
  | Extension_set
    :  [`Enabled|`Disabled] * string list
    -> unit request
  | Path
    :  [`Build|`Source] 
     * [`Relative|`Absolute]
     * [`Add|`Rem]
     * string list
    -> bool request
  | Path_list 
    :  [`Build|`Source] 
    -> string list request
  | Path_reset
    :  [`Build|`Source|`Both] 
    -> unit request
  | Project_load
    :  [`File|`Find] * string
    -> string list request
type a_request = Request : 'a request -> a_request

type response = 
  | Return    : 'a request * 'a -> response
  | Failure   : string -> response
  | Error     : Json.json -> response
  | Exception : exn -> response
