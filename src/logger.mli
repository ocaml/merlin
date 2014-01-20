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

(** A logging module. *)

module Section : sig
  type t = [
    | `protocol
    | `locate
    | `completion
    | `dot_merlin
  ]

  val of_string : string -> t
  val to_string : t -> string
end

val set_default_destination : string -> unit

val monitor : ?dest:string -> Section.t -> unit
(** [monitor ?dest section] starts the monitoring of [section].
    If [dest] is set then all subsequent logging related to [section] will go in
    that file, in the default destination otherwise.

    If [dest] is omitted and the default destination is not set,
    [Invalid_argument] is raised. *)

val is_monitored : Section.t -> bool

val forget : Section.t -> unit
(** [forget section] stops the monitoring of [section] *)

val log : Section.t -> ?prefix:string -> string -> unit
(** [log section msg] will output [msg] on the channel dedicated to [section],
    if it is being monitored. *)

val error : Section.t -> string -> unit
(** [error section msg] behaves as [log] if [section] is being monitored, but
    prints to the default_destination (if it is set) otherwise. *)

val shutdown : unit -> unit
(** Closes all the open channels, unsets the default destination and stops all
    monitoring. *)
