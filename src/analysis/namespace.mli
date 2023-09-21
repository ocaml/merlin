type t = [
  | `Vals
  | `Type
  | `Constr
  | `Mod
  | `Modtype
  | `Functor
  | `Labels
  | `Unknown
  | `Apply
]

val to_tag_string : t -> string

val to_string : t -> string
