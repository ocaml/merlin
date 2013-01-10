type item = Chunk.sync * Env.t
type sync = item History.sync
type t = item History.t

val sync : Chunk.t -> t -> t
