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

(** Parse dot-merlin files **)

(* A dot-merlin file is made of one or more directive and an optional project
   name *)
type directive = [
  | `B   of string
  | `S   of string
  | `CMI of string
  | `CMT of string
  | `EXT of string list
  | `FLG of string
  | `PKG of string list
  | `STDLIB of string
  | `FINDLIB of string
]

type file = {
  project    : string option;
  path       : string;
  directives : directive list;
}

(* After parsing, dot-merlins are turned into a project configuration *)
type config = {
  dot_merlins : string list;
  build_path  : string list;
  source_path : string list;
  cmi_path    : string list;
  cmt_path    : string list;
  packages    : string list;
  flags       : string list list;
  extensions  : string list;
  stdlib      : string;
}

(* Find path of the dot-merlin file *)
val find : string -> string option

(* Parse a file from the filesystem. Path is a filename *)
val read : ?tail:file List.Lazy.t lazy_t -> string -> file List.Lazy.t
val empty_config : config
val parse : ?config:config -> file List.Lazy.t -> config

(* If any of the dot-merlin specify a project name, return it *)
val project_name : file List.Lazy.t -> string option
val path_of_packages : string list
  -> [> `Failures of (string * exn) list | `Ok ] * string list * Ppxsetup.t
