(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013  Frédéric Bour  <frederic.bour(_)lakaban.net>
                      Thomas Refis  <refis.thomas(_)gmail.com>
                      Simon Castellan  <simon.castellan(_)iuwt.fr>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

open Std

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
    :  [`Definitions of int | `Source of string | `More of string | `End]
    -> position option request
  | Type_expr
    :  string * position option
    -> string request
  | Type_enclosing
    :  (string * int) * position
    -> (Location.t * string) list request
  | Complete_prefix
    :  string * position option
    -> completion list request
  | Locate
    :  string * position option
    -> [ `Found of string option * Lexing.position
       | `Not_in_env of string
       | `File_not_found of string
       | `Not_found
       ] request
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
  | Errors
    :  exn list request
  | Dump
    :  [`Env of [`Normal|`Full] * position option|`Sig|`Chunks|`Tree|`Outline|`Exn|`History]
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
    -> bool request
  | Path_list
    :  [`Build|`Source]
    -> string list request
  | Path_reset
    :  unit request
  | Project_load
    :  [`File|`Find] * string
    -> (string list * [`Ok | `Failures of (string * exn) list]) request
  | Occurences
    :  [ `Ident_at of position ]
    -> Location.t list request

type a_request = Request : 'a request -> a_request

type response =
  | Return    : 'a request * 'a -> response
  | Failure   : string -> response
  | Error     : Json.json -> response
  | Exception : exn -> response
