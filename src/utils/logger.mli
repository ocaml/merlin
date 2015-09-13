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

(** Log module
  *
  * 1. Provide functions to log arbitrary messages, filtered according to a
  * section and a verbosity level.
  *
  * 2. Allow to setup destinations for these log messages.
  *
  * MERLIN_LOG environment variable and -debug commandline flag are used to
  * determine destinations.
  *
  **)

open Std

(** Verbosity **)

type level = [ `error | `info | `debug ]

(** Module sections, to group messages along functional lines. *)

module Section : sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val enabled : level -> t -> bool

  val general : t
  val project_load : t

  val list : unit -> t list
end

type section = Section.t
val section : string -> section
val general : section

val set_default_destination : string -> unit

val monitor : ?dest:string -> section -> level -> unit
(** [monitor ?dest section] starts the monitoring of [section].
    If [dest] is set then all subsequent logging related to [section] will go in
    that file, in the default destination otherwise.

    If [dest] is omitted and the default destination is not set,
    [Invalid_argument] is raised. *)

val is_monitored : section -> bool

val forget : section -> unit
(** [forget section] stops the monitoring of [section] *)

val log : level -> section -> ?title:string -> string -> unit
val logf : level -> section -> ?title:string ->(Format.formatter -> 'a -> unit) -> 'a -> unit
val logj : level -> section -> ?title:string -> Json.json -> unit
val logjf : level -> section -> ?title:string ->('a -> Json.json) -> 'a -> unit

val error : section -> ?title:string -> string -> unit
val errorf : section -> ?title:string -> (Format.formatter -> 'a -> unit) -> 'a -> unit
val errorj : section -> ?title:string -> Json.json -> unit
val errorjf : section -> ?title:string -> ('a -> Json.json) -> 'a -> unit
(** [error section msg] behaves as [log] if [section] is being monitored, but
    prints to the default_destination (if it is set) otherwise. *)

val info : section -> ?title:string -> string -> unit
val infof : section -> ?title:string -> (Format.formatter -> 'a -> unit) -> 'a -> unit
val infoj : section -> ?title:string -> Json.json -> unit
val infojf : section -> ?title:string -> ('a -> Json.json) -> 'a -> unit
(** [info section msg] will output [msg] on the channel dedicated to [section],
    if it is being monitored. *)

val debug : section -> ?title:string -> string -> unit
val debugf : section -> ?title:string -> (Format.formatter -> 'a -> unit) -> 'a -> unit
val debugj : section -> ?title:string -> Json.json -> unit
val debugjf : section -> ?title:string -> ('a -> Json.json) -> 'a -> unit
(** Use [debug section msg] for mostly unimportant messages, those will be
    displayed only if verbose. *)

val shutdown : unit -> unit
(** Closes all the open channels, unsets the default destination and stops all
    monitoring. *)

(** Messages targeted to the editor *)
val tell_editor : string -> unit
val with_editor : string list ref -> (unit -> 'a) -> 'a
