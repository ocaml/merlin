type t
type diff

type info = {
  path: string list;
  index: int;
  locations: Location.t list;
}

val empty : t
val empty_diff: diff

val update : Merlin_typer.content -> diff * t -> diff * t

val get_diff: diff -> info list
