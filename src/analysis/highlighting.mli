type t

type info = {
  color_index: string list * int;
  locations: Location.t list;
}

val empty : t
val update : Merlin_typer.content -> t -> info list * t
