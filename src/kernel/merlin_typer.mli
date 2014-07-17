type t

val is_valid : t -> bool

val fresh : unit_name:string -> stamp:bool ref -> Extension.set -> t
val update : Merlin_parser.t -> t -> t

val env : t -> Env.t
val structures : t -> Typedtree.structure list
val exns : t -> exn list
val extensions : t -> Extension.set

val dump : Format.formatter -> t -> unit

val manual
  :  t
  -> [ `sg  of Parsetree.signature
     | `str of Parsetree.structure ]
  -> t

val with_typer : t -> (t -> 'a) -> 'a
