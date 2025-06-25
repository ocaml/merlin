module For_deriving = struct
  type t =
    | Value [@rename "Value"]
    | Constructor [@rename "Constructor"]
    | Label [@rename "Label"]
    | Module [@rename "Module"]
    | Modtype [@rename "Signature"]
    | Type [@rename "Type"]
    | Exn [@rename "Exn"]
    | Class [@rename "Class"]
    | ClassType [@rename "ClassType"]
    | Method [@rename "Method"]
  [@@deriving string]

  let to_poly = function
    | Value -> `Value
    | Constructor -> `Constructor
    | Label -> `Label
    | Module -> `Module
    | Modtype -> `Modtype
    | Type -> `Type
    | Exn -> `Exn
    | Class -> `Class
    | ClassType -> `ClassType
    | Method -> `Method

  let of_poly = function
    | `Value -> Value
    | `Constructor -> Constructor
    | `Label -> Label
    | `Module -> Module
    | `Modtype -> Modtype
    | `Type -> Type
    | `Exn -> Exn
    | `Class -> Class
    | `ClassType -> ClassType
    | `Method -> Method
end

type t =
  [ `Value
  | `Constructor
  | `Label
  | `Module
  | `Modtype
  | `Type
  | `Exn
  | `Class
  | `ClassType
  | `Method ]
[@@deriving equal, enumerate]

let to_string x = For_deriving.of_poly x |> For_deriving.to_string
let of_string s = For_deriving.of_string s |> For_deriving.to_poly
