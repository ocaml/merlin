module Section : sig
  type t = [
    | `protocol
    | `locate
    | `completion
  ]

  val of_string : string -> t
  val to_string : t -> string
end

val set_default_destination : out_channel -> unit

val monitor : ?dest:out_channel -> Section.t -> unit

val is_monitored : Section.t -> bool

val forget : Section.t -> unit

val log : Section.t -> ?prefix:string -> string -> unit

val error : Section.t -> string -> unit
