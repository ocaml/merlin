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
val logj : level -> section -> ?title:string ->Std.json -> unit
val logjf : level -> section -> ?title:string ->('a -> Std.json) -> 'a -> unit

val info : section -> ?title:string -> string -> unit
val infof : section -> ?title:string -> (Format.formatter -> 'a -> unit) -> 'a -> unit
val infoj : section -> ?title:string -> Std.json -> unit
val infojf : section -> ?title:string -> ('a -> Std.json) -> 'a -> unit
(** [info section msg] will output [msg] on the channel dedicated to [section],
    if it is being monitored. *)

val error : section -> ?title:string -> string -> unit
val errorf : section -> ?title:string -> (Format.formatter -> 'a -> unit) -> 'a -> unit
val errorj : section -> ?title:string -> Std.json -> unit
val errorjf : section -> ?title:string -> ('a -> Std.json) -> 'a -> unit
(** [error section msg] behaves as [log] if [section] is being monitored, but
    prints to the default_destination (if it is set) otherwise. *)

val debug : section -> ?title:string -> string -> unit
val debugf : section -> ?title:string -> (Format.formatter -> 'a -> unit) -> 'a -> unit
val debugj : section -> ?title:string -> Std.json -> unit
val debugjf : section -> ?title:string -> ('a -> Std.json) -> 'a -> unit
(** Use [debug section msg] for mostly unimportant messages, those will be
    displayed only if verbose. *)

val shutdown : unit -> unit
(** Closes all the open channels, unsets the default destination and stops all
    monitoring. *)
