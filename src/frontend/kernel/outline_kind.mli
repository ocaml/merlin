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
[@@deriving string, equal, enumerate]
