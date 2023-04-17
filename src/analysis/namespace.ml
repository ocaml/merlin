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

let to_tag_string = function
  | `Mod -> ""
  | `Functor -> "[functor]"
  | `Labels -> "[label]"
  | `Constr -> "[cstr]"
  | `Type -> "[type]"
  | `Vals -> "[val]"
  | `Modtype -> "[Mty]"
  | `Unknown -> "[?]"
  | `Apply -> "[functor application]"

let to_string = function
  | `Mod -> "(module) "
  | `Functor -> "(functor)"
  | `Labels -> "(label) "
  | `Constr -> "(constructor) "
  | `Type -> "(type) "
  | `Vals -> "(value) "
  | `Modtype -> "(module type) "
  | `Unknown -> "(unknown)"
  | `Apply -> "(functor application)"

