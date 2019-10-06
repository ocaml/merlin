type t

val of_yojson : Yojson.Safe.t -> (t, string) result
val to_yojson : t -> Yojson.Safe.t

val to_path : t -> string
val of_path : string -> t

val to_string : t -> string

val pp : Format.formatter -> t -> unit
