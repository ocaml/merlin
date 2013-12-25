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
