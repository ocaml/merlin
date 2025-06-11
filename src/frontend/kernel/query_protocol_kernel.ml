(* This module contains definitions that can be used in a js-of-ocaml environment. This
   is useful because it allows VSCode extensions (which run in javascript) to use the
   serializers/deserializers defined in this module. *)

open struct
  include Ppx_yojson_conv_lib.Yojson_conv.Primitives

  module Lexing = struct
    include Lexing

    type nonrec position = position =
      { pos_fname : string; pos_lnum : int; pos_bol : int; pos_cnum : int }
    [@@deriving yojson]
  end
end

module Locate_type_multi_result = struct
  open Ppx_yojson_conv_lib.Yojson_conv.Primitives

  type node_data =
    | Arrow
    | Tuple
    | Object
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
end
