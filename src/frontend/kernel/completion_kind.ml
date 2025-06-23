type t =
  [ `Constructor
  | `Labels
  | `Modules
  | `Modules_type
  | `Types
  | `Values
  | `Variants
  | `Keywords ]

let to_string = function
  | `Constructor -> "constructor"
  | `Keywords -> "keywords"
  | `Labels -> "label"
  | `Modules -> "module"
  | `Modules_type -> "module-type"
  | `Types -> "type"
  | `Values -> "value"
  | `Variants -> "variant"

let of_string_opt = function
  | "t" | "type" | "types" -> Some `Types
  | "v" | "val" | "value" | "values" -> Some `Values
  | "variant" | "variants" | "var" -> Some `Variants
  | "c" | "constr" | "constructor" -> Some `Constructor
  | "l" | "label" | "labels" -> Some `Labels
  | "m" | "mod" | "module" -> Some `Modules
  | "mt" | "modtype" | "module-type" -> Some `Modules_type
  | "k" | "kw" | "keyword" | "keywords" -> Some `Keywords
  | _ -> None
