open Extend_protocol (** Helper for the driver (Merlin) *)


type t

exception Extension of string * string * string

val run : ?notify:(string -> unit) -> ?debug:(string -> unit) -> string -> t
val stop : t -> unit
val capabilities : t -> capabilities
val reader : t -> Reader.request -> Reader.response
