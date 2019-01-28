type t

val of_yojson : Yojson.Safe.json -> (t, string) result
val to_yojson : t -> Yojson.Safe.json

val to_path : t -> string
val of_path : string -> t

val pp : Format.formatter -> t -> unit
