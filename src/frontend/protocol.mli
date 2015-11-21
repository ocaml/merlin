(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
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

type protocol_version = [ `V2 ]

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

type shape = {
  shape_loc : Location.t;
  shape_sub : shape list;
}

type is_tail_position = [`No | `Tail_position | `Tail_call]

type context = [`ML | `MLI | `Auto ] * string option * string list option

type _ query_command =
  | Type_expr
    :  string * position option
    -> string query_command
  | Type_enclosing
    :  (string * int) option * position
    -> (Location.t * string * is_tail_position) list query_command
  | Enclosing
    :  position
    -> Location.t list query_command
  | Complete_prefix
    :  string * position * bool
    -> completions query_command
  | Expand_prefix
    :  string * position
    -> completions query_command
  | Document
    : string option * position
    -> [ `Found of string
       | `Invalid_context
       | `Builtin of string
       | `Not_in_env of string
       | `File_not_found of string
       | `Not_found of string * string option
       | `No_documentation
       ] query_command
  | Locate
    : string option * [ `ML | `MLI ] * position
    -> [ `Found of string option * Lexing.position
       | `Invalid_context
       | `Builtin of string
       | `Not_in_env of string
       | `File_not_found of string
       | `Not_found of string * string option
       | `At_origin
       ] query_command
  | Jump
    : string * position
    -> [ `Found of Lexing.position
       | `Error of string
       ] query_command
  | Case_analysis
    : Location.t -> (Location.t * string) query_command
  | Outline
    :  outline query_command
  | Shape
    :  Lexing.position -> shape list query_command
  | Errors
    :  Error_report.t list query_command
  | Dump
    :  Json.json list
    -> Json.json query_command
  | Which_path
    :  string list
    -> string query_command
  | Which_with_ext
    :  string list
    -> string list query_command
  | Flags_get
    :  string list query_command
  | Findlib_list
    :  string list query_command
  | Extension_list
    :  [`All|`Enabled|`Disabled]
    -> string list query_command
  | Path_list
    :  [`Build|`Source]
    -> string list query_command
  | Project_get
    :  (string list * [`Ok | `Failures of (string * exn) list]) query_command
  | Occurrences
    : [`Ident_at of position]
    -> Location.t list query_command
  | Version
    : string query_command
  | Idle_job
    : bool query_command

type _ sync_command =
  | Tell
    : [ `Start of position option
      | `Source of string | `File of string
      | `Source_eof of string | `File_eof of string
      | `Eof | `Marker]
    -> cursor_state sync_command
  | Drop
    :  cursor_state sync_command
  | Seek
    :  [`Marker|`Position|`End|`Before of position|`Exact of position]
    -> cursor_state sync_command
  | Refresh
    :  unit sync_command
  | Flags_set
    :  string list
    -> [ `Ok | `Failures of (string * exn) list ] sync_command
  | Findlib_use
    :  string list
    -> [`Ok | `Failures of (string * exn) list] sync_command
  | Extension_set
    :  [`Enabled|`Disabled] * string list
    -> [`Ok | `Failures of (string * exn) list] sync_command
  | Path
    :  [`Build|`Source]
     * [`Add|`Rem]
     * string list
    -> unit sync_command
  | Path_reset
    :  unit sync_command
  | Protocol_version
    : int option
    -> ([`Selected of protocol_version] *
        [`Latest of protocol_version] *
        string) sync_command
  | Checkout
    : context
    -> cursor_state sync_command

type 'a command =
  | Query of 'a query_command
  | Sync  of 'a sync_command

type request =
  | Request          : 'a command -> request
  | Context_request  : context * 'a command -> request

type response =
  | Return    : 'a command * 'a -> response
  | Failure   : string -> response
  | Error     : Json.json -> response
  | Exception : exn -> response
