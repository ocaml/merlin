(** merlin: manage all internal state *)

type cache

val new_cache : unit -> cache
val cache : cache ref
