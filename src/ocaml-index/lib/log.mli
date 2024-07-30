module Level : sig
  type t = Debug | Warning | Error
end

val set_log_level : Level.t -> unit
val log : level:Level.t -> ('a, Format.formatter, unit, unit) format4 -> 'a
val debug : ('a, Format.formatter, unit, unit) format4 -> 'a
val warn : ('a, Format.formatter, unit, unit) format4 -> 'a
val error : ('a, Format.formatter, unit, unit) format4 -> 'a
