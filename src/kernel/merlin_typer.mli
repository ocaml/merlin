type t

val is_valid : t -> bool

val fresh : unit_name:string -> stamp:bool ref -> Extension.set -> t
val update : Merlin_parser.t -> t -> t

val env : t -> Env.t
val contents : t -> [`Str of Typedtree.structure | `Sg of Typedtree.signature] list
val exns : t -> exn list
val extensions : t -> Extension.set

val dump : Format.formatter -> t -> unit

val with_typer : t -> (t -> 'a) -> 'a
