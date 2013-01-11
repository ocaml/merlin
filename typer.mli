type item = Chunk.sync * Env.t * exn list
type sync = item History.sync
type t = item History.t

val sync : Chunk.t -> t -> t
