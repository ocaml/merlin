type t =
  [ `Constructor
  | `Labels
  | `Modules
  | `Modules_type
  | `Types
  | `Values
  | `Variants
  | `Keywords ]

val to_string : t -> string
val of_string_opt : string -> t option
