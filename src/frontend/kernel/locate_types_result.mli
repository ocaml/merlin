type node_data =
  | Arrow
  | Tuple
  | Object
  | Poly_variant
  | Type_ref of
      { type_ : string;
        result :
          [ `Found of string option * Lexing.position
          | `Builtin of string
          | `Not_in_env of string
          | `File_not_found of string
          | `Not_found of string * string option ]
      }
[@@deriving yojson]

type type_tree = { data : node_data; children : type_tree list }
[@@deriving yojson]

type t = Success of type_tree | Invalid_context [@@deriving yojson]
