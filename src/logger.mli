module Section : sig
  type t = [
    | `protocol
    | `locate
    | `completion
  ]

  val of_string : string -> t
  val to_string : t -> string
end

val set_default_destination : string -> unit

val monitor : ?dest:string -> Section.t -> unit

val is_monitored : Section.t -> bool

val forget : Section.t -> unit

val log : Section.t -> ?prefix:string -> string -> unit

val error : Section.t -> string -> unit

val shutdown : unit -> unit
