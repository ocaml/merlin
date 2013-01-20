type item = Chunk.sync * (Env.t * (Typedtree.structure * Types.signature) list * exn list)
type sync = item History.sync
type t = item History.t

val initial_env : Env.t Lazy.t
val env : t -> Env.t
val trees : t -> (Typedtree.structure * Types.signature) list
val sync : Chunk.t -> t -> t
