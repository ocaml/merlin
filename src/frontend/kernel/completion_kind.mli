module In : sig
  type t =
    [ `Constructor
    | `Labels
    | `Modules
    | `Modules_type
    | `Types
    | `Values
    | `Variants
    | `Keywords ]
  [@@deriving to_string, enumerate, equal]

  val of_string_opt : string -> t option
end

module Out : sig
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
  [@@deriving string, enumerate, equal]
end
