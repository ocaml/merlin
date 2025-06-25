module In = struct
  type t =
    [ `Constructor
    | `Labels
    | `Modules
    | `Modules_type
    | `Types
    | `Values
    | `Variants
    | `Keywords ]
  [@@deriving enumerate, equal]

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
end

module Out = struct
  (* CR-someday: This module is necessary because ppx_string_conv doesn't currently
     (v0.17.0) support polymorphic variants. *)
  module For_deriving = struct
    type t =
      | Value [@rename "Value"]
      | Constructor [@rename "Constructor"]
      | Variant [@rename "Variant"]
      | Label [@rename "Label"]
      | Module [@rename "Module"]
      | Modtype [@rename "Signature"]
      | Type [@rename "Type"]
      | MethodCall [@rename "#"]
      | Keyword [@rename "Keyword"]
    [@@deriving string]

    let to_poly = function
      | Value -> `Value
      | Constructor -> `Constructor
      | Variant -> `Variant
      | Label -> `Label
      | Module -> `Module
      | Modtype -> `Modtype
      | Type -> `Type
      | MethodCall -> `MethodCall
      | Keyword -> `Keyword

    let of_poly = function
      | `Value -> Value
      | `Constructor -> Constructor
      | `Variant -> Variant
      | `Label -> Label
      | `Module -> Module
      | `Modtype -> Modtype
      | `Type -> Type
      | `MethodCall -> MethodCall
      | `Keyword -> Keyword
  end

  type t =
    [ `Value
    | `Constructor
    | `Variant
    | `Label
    | `Module
    | `Modtype
    | `Type
    | `MethodCall
    | `Keyword ]
  [@@deriving enumerate, equal]

  let to_string x = For_deriving.of_poly x |> For_deriving.to_string
  let of_string s = For_deriving.of_string s |> For_deriving.to_poly
end
