type t

val t_of_yojson : Yojson.Safe.t -> t
val yojson_of_t : t -> Yojson.Safe.t

val to_path : t -> string
val of_path : string -> t

val to_string : t -> string

val pp : Format.formatter -> t -> unit
