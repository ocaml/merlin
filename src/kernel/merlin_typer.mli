type t

val fresh: Extension.set -> t
val update: Merlin_parser.t -> t -> t

val env: t -> Env.t
val structures: t -> Typedtree.structure list
val exns: t -> exn list
