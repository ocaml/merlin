type t

val is_valid: t -> bool

val fresh: Extension.set -> t
val update: Merlin_parser.t -> t -> t

val env: t -> Env.t
val structures: t -> Typedtree.structure list
val exns: t -> exn list
val extensions: t -> Extension.set
