type t

val empty: t
val add_ppx: string -> t -> t
val add_ppxopts: string -> string list -> t -> t

val union: t -> t -> t

val command_line: t -> string list

val dump : t -> Std.json
