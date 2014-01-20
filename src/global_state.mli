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

open Misc

(* Project configuration *)
module Project : sig
  (* Current buffer path *)
  val set_local_path : string -> unit

  (* Project-wide configuration *)
  val set_dot_merlin
    : Dot_merlin.path_config -> [`Ok | `Failures of (string * exn) list]

  val reset_project : unit -> unit

  (* Config override by user *)
  val reset_user : unit -> unit
  val user_path : action:[`Add | `Rem] ->
                  var:[`Build | `Source] ->
                  ?cwd:string -> string -> unit

  val user_load_packages
    : string list -> [`Ok | `Failures of (string * exn) list]
  val user_set_extension : enabled:bool -> string -> unit

  (* Output values *)
  val source_path : Path_list.t
  val build_path  : Path_list.t
  val cmt_path    : Path_list.t

  (* List all top modules of current project *)
  val global_modules : unit -> string list
  (* Force recomputing list of global modules *)
  val flush_global_modules : unit -> unit
end
