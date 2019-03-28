open Protocol_conv_json

type t [@@deriving protocol ~driver:(module Json)]

val to_path : t -> string
val of_path : string -> t

val to_string : t -> string

val pp : Format.formatter -> t -> unit
