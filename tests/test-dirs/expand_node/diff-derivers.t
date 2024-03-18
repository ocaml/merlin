Test with ppx_deriving_yojson  
  $ cat >dune-project <<EOF
  > (lang dune 2.9)
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name apx)
  >  (libraries yojson)
  >  (preprocess (pps ppx_deriving_yojson)))
  > EOF

  $ cat > apx.ml << EOF
  > type t = int [@@deriving yojson]
  > EOF

  $ dune build

cursor is on "deriving"
expand-node returns expression as a leaf node when cursor is on deriving
  $ $MERLIN single expand-node -position 1:17 -filename ./apx.ml < ./apx.ml
  Parsetree nodes: [ expression; value_binding; structure_item; structure ]
  Typedtree nodes: [ expression; value_binding; structure_item; structure ]
  {
    "class": "return",
    "value": "No deriver on this node",
    "notifications": []
  }

cursor is on "type"
expand-node returns expression as a leaf node when cursor is on the keyword type
  $ $MERLIN single expand-node -position 1:3 -filename ./apx.ml < ./apx.ml
  Parsetree nodes: [ expression; value_binding; structure_item; structure ]
  Typedtree nodes: [ expression; value_binding; structure_item; structure ]
  {
    "class": "return",
    "value": "No deriver on this node",
    "notifications": []
  }


Test with ppx_yojson_conv
  $ cat >dune-project <<EOF
  > (lang dune 2.9)
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name apt)
  >  (libraries yojson)
  >  (preprocess (pps ppx_yojson_conv)))
  > EOF

  $ cat > apt.ml << EOF
  > open Ppx_yojson_conv_lib.Yojson_conv.Primitives
  > type t = int [@@deriving yojson]
  > EOF

  $ dune build

cursor is on "deriving"
expand-node returns core_type as a leaf node when cursor is on deriving
  $ $MERLIN single expand-node -position 2:17 -filename ./apt.ml < ./apt.ml
  Parsetree nodes: [ core_type; type_declaration; structure_item; structure ]
  Typedtree nodes: [ core_type; type_declaration; structure_item; structure ]
  {
    "class": "return",
    "value": "[ type t = int[@@deriving yojson]
  ; include
    struct
      let _ = fun (_ : t) -> ()
      let t_of_yojson =
        (int_of_yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)
      let _ = t_of_yojson
      let yojson_of_t =
        (yojson_of_int : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)
      let _ = yojson_of_t
    end[@@ocaml.doc \"@inline\"][@@merlin.hide ]
   ]",
    "notifications": []
  }

cursor is on type
expand-node returns type_kind as a leaf node when cursor is on the keyword type
  $ $MERLIN single expand-node -position 2:3 -filename ./apt.ml < ./apt.ml
  Parsetree nodes: [ type_kind; type_declaration; structure_item; structure ]
  Typedtree nodes: [ type_kind; type_declaration; structure_item; structure ]
  {
    "class": "return",
    "value": "No deriver on this node",
    "notifications": []
  }
