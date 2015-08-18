(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2014  Frédéric Bour  <frederic.bour(_)lakaban.net>
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
open Merlin_lib

type position = Lexing.position
type cursor_state = {
  cursor: position;
  marker: bool;
}

module Compl : sig
  type entry = {
    name: string;
    kind: [`Value|`Constructor|`Variant|`Label|
           `Module|`Modtype|`Type|`MethodCall];
    desc: string;
    info: string;
  }

  type application_context = {
    argument_type: string;
    labels : (string * string) list;
  }

  type t = {
    entries: entry list;
    context: [ `Unknown
             | `Application of application_context
             ]
  }
end
type completions = Compl.t

type outline = item list
and item = {
  outline_name : string ;
  outline_kind : [
    | `Value
    | `Constructor
    | `Label
    | `Module
    | `Modtype
    | `Type
    | `Exn
    | `Class
    | `Method
  ];
  location : Location.t ;
  children : outline ;
}

type is_tail_position = [`No | `Tail_position | `Tail_call]

type context = [`ML | `MLI | `Auto ] * string option * string list option

type _ request =
  | Tell
    : [ `Start of position option
      | `Source of string | `File of string
      | `Source_eof of string | `File_eof of string
      | `Eof | `Marker]
    -> cursor_state request
  | Type_expr
    :  string * position option
    -> string request
  | Type_enclosing
    :  (string * int) option * position
    -> (Location.t * string * is_tail_position) list request
  | Enclosing
    :  position
    -> Location.t list request
  | Complete_prefix
    :  string * position * bool
    -> completions request
  | Expand_prefix
    :  string * position
    -> completions request
  | Document
    : string option * position
    -> [ `Found of string
       | `Invalid_context
       | `Not_in_env of string
       | `File_not_found of string
       | `Not_found of string * string option
       | `No_documentation
       ] request
  | Locate
    : string option * [ `ML | `MLI ] * position
    -> [ `Found of string option * Lexing.position
       | `Invalid_context
       | `Not_in_env of string
       | `File_not_found of string
       | `Not_found of string * string option
       | `At_origin
       ] request
  | Case_analysis
    : Location.t -> (Location.t * string) request
  | Outline
    :  outline request
  | Drop
    :  cursor_state request
  | Seek
    :  [`Marker|`Position|`End|`Before of position|`Exact of position]
    -> cursor_state request
  | Boundary
    :  [`Prev|`Next|`Current] * position
    -> Location.t option request
  | Checkout
    :  context
    -> cursor_state request
  | Refresh
    :  unit request
  | Errors
    :  Error_report.t list request
  | Dump
    :  [`Env of [`Normal|`Full] * position option | `Flags | `Warnings
       |`Sig|`Parser|`Exn|`Browse|`Recover|`Typer of [`Input|`Output] | `Tokens]
    -> Json.json request
  | Which_path
    :  string list
    -> string request
  | Which_with_ext
    :  string list
    -> string list request
  | Flags
    : [ `Add of string list | `Clear ]
    -> [ `Ok | `Failures of (string * exn) list ] request
  | Flags_get
    :  string list list request
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
    -> [`Ok | `Failures of (string * exn) list] request
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
  | Project_get
    :  (string list * [`Ok | `Failures of (string * exn) list]) request
  | Occurrences
    : [`Ident_at of position]
    -> Location.t list request
  | Idle_job
    : bool request
  | Version
    : string request

type a_request = Request : 'a request -> a_request
               | Context_request : context * 'a request -> a_request

type response =
  | Return    : 'a request * 'a -> response
  | Failure   : string -> response
  | Error     : Json.json -> response
  | Exception : exn -> response
