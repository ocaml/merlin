open Std
open Merlin_lib

type path = Parser.Path.path
type position = Lexing.position

type completion = {
  name: string;
  kind: [`Value|`Constructor|`Label|
         `Module|`Modtype|`Type|`MethodCall];
  desc: string;
  info: string;
}

type outline = item list
and item = {
  name : string ;
  kind : [`Value|`Constructor|`Label|`Module|`Modtype|`Type|`Exn] ;
  pos  : position ;
  children : outline ;
}

type _ request =
  | Tell
    : [`Start|`Source of string]
    -> (position * path)  request
  | Type_expr
    :  string * position option
    -> string request
  | Type_enclosing
    :  (string * int) * position
    -> (Location.t * string) list request
  | Complete_prefix
    :  string * position
    -> completion list request
  | Locate
    :  string * position option
    -> (string option * position) option request
  | Outline
    :  outline request
  | Drop
    :  (position * path) request
  | Seek
    :  [`Position|`End|`Before of position|`Exact of position]
    -> (position * path) request
  | Boundary
    :  [`Prev|`Next|`Current] * position option
    -> Location.t option request
  | Reset
    :  [`ML | `MLI] * string option
    -> (position * path) request
  | Refresh
    :  unit request
  | Errors
    :  exn list request
  | Dump
    :  [`Env of [`Normal|`Full] * position option|`Sig|`Parser|`Exn|`History]
    -> Json.json request
  | Which_path
    :  string
    -> string request
  | Which_with_ext
    :  string
    -> string list request
  | Findlib_use
    :  string list
    -> [`Ok | `Failures of (string * exn) list] request
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
     * [`Add|`Rem]
     * string list
    -> unit request
  | Path_reset
    :  unit request
  | Path_list
    :  [`Build|`Source]
    -> string list request
  | Project_load
    :  [`File|`Find]
     * string
    -> (string list * [`Ok | `Failures of (string * exn) list]) request

type a_request = Request : 'a request -> a_request

type response =
  | Return    : 'a request * 'a -> response
  | Failure   : string -> response
  | Error     : Json.json -> response
  | Exception : exn -> response
