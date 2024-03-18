  $ cat >dune-project <<EOF
  > (lang dune 2.9)
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name apx)
  >  (libraries yojson)
  >  (preprocess (pps ppx_yojson_conv)))
  > EOF

  $ cat > apx.ml << EOF
  > open Ppx_yojson_conv_lib.Yojson_conv.Primitives
  > type tttttt = int [@@deriving yojson]
  > type point = {x:float; y:float} [@@deriving yojson]
  > EOF

  $ dune build
# expand-node will trigger only if the cursor is within the range of the attribute name, in this case "deriving", not on the payload.
# This is a current design choice.
# TODO: Think through if this choice is good. 

cursor is on type
type tttttt = int [@@deriving yojson]
expand-node returns `type_kind` as its leaf, when the position points to a `type` keyword in a type declaration
  $ $MERLIN single expand-node -position 2:2 -filename ./apx.ml < ./apx.ml
  Parsetree nodes: [ type_kind; type_declaration; structure_item; structure ]
  Typedtree nodes: [ type_kind; type_declaration; structure_item; structure ]
  {
    "class": "return",
    "value": "No deriver on this node",
    "notifications": []
  }

cursor is on tttttt
expand-node returns `type_kind` as its leaf, when the position points to a `type` keyword in a type declaration
  $ $MERLIN single expand-node -position 2:7 -filename ./apx.ml < ./apx.ml
  Parsetree nodes: [ type_kind; type_declaration; structure_item; structure ]
  Typedtree nodes: [ type_kind; type_declaration; structure_item; structure ]
  {
    "class": "return",
    "value": "No deriver on this node",
    "notifications": []
  }

cursor is on int
expand-node returns `core_type` as its leaf, when the position points to the core type in a type declaration
  $ $MERLIN single expand-node -position 2:17 -filename ./apx.ml < ./apx.ml
  Parsetree nodes: [ core_type; type_declaration; structure_item; structure ]
  Typedtree nodes: [ core_type; type_declaration; structure_item; structure ]
  {
    "class": "return",
    "value": "No deriver on this node",
    "notifications": []
  }

cursor is on "deriving"
expand-node returns `core_type` as its leaf, when the position points to the the name of an attribute (e.g. "deriving")
  $ $MERLIN single expand-node -position 2:25 -filename ./apx.ml < ./apx.ml
  Parsetree nodes: [ core_type; type_declaration; structure_item; structure ]
  Typedtree nodes: [ core_type; type_declaration; structure_item; structure ]
  {
    "class": "return",
    "value": "[ type tttttt = int[@@deriving yojson]
  ; include
    struct
      let _ = fun (_ : tttttt) -> ()
      let tttttt_of_yojson =
        (int_of_yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> tttttt)
      let _ = tttttt_of_yojson
      let yojson_of_tttttt =
        (yojson_of_int : tttttt -> Ppx_yojson_conv_lib.Yojson.Safe.t)
      let _ = yojson_of_tttttt
    end[@@ocaml.doc \"@inline\"][@@merlin.hide ]
   ]",
    "notifications": []
  }


cursor is on "yojson"
expand-node returns `core_type` as its leaf, when the position points to the the payload of an attribute (e.g. "yojson")
  $ $MERLIN single expand-node -position 2:34 -filename ./apx.ml < ./apx.ml
  Parsetree nodes: [ core_type; type_declaration; structure_item; structure ]
  Typedtree nodes: [ core_type; type_declaration; structure_item; structure ]
  {
    "class": "return",
    "value": "No deriver on this node",
    "notifications": []
  }

cursor is on "deriving"
expand-node returns `core_type` as its leaf, when the position points to the name of the attribute (e.g. "deriving")
  $ $MERLIN single expand-node -position 3:39 -filename ./apx.ml < ./apx.ml
  Parsetree nodes: [ core_type; label_declaration; type_kind; type_declaration; structure_item; structure ]
  Typedtree nodes: [ core_type; core_type; label_declaration; type_kind; type_declaration; structure_item; structure ]
  {
    "class": "return",
    "value": "[ type point = {
    x: float ;
    y: float }[@@deriving yojson]
  ; include
    struct
      let _ = fun (_ : point) -> ()
      let point_of_yojson =
        (let _tp_loc = \"apx.ml.point\" in
         function
         | `Assoc field_yojsons as yojson ->
             let x_field = ref Ppx_yojson_conv_lib.Option.None
             and y_field = ref Ppx_yojson_conv_lib.Option.None
             and duplicates = ref []
             and extra = ref [] in
             let rec iter =
               function
               | (field_name, _field_yojson)::tail ->
                   ((match field_name with
                     | \"x\" ->
                         (match Ppx_yojson_conv_lib.(!) x_field with
                          | Ppx_yojson_conv_lib.Option.None ->
                              let fvalue = float_of_yojson _field_yojson in
                              x_field :=
                                (Ppx_yojson_conv_lib.Option.Some fvalue)
                          | Ppx_yojson_conv_lib.Option.Some _ ->
                              duplicates := (field_name ::
                                (Ppx_yojson_conv_lib.(!) duplicates)))
                     | \"y\" ->
                         (match Ppx_yojson_conv_lib.(!) y_field with
                          | Ppx_yojson_conv_lib.Option.None ->
                              let fvalue = float_of_yojson _field_yojson in
                              y_field :=
                                (Ppx_yojson_conv_lib.Option.Some fvalue)
                          | Ppx_yojson_conv_lib.Option.Some _ ->
                              duplicates := (field_name ::
                                (Ppx_yojson_conv_lib.(!) duplicates)))
                     | _ ->
                         if
                           Ppx_yojson_conv_lib.(!)
                             Ppx_yojson_conv_lib.Yojson_conv.record_check_extra_fields
                         then
                           extra := (field_name ::
                             (Ppx_yojson_conv_lib.(!) extra))
                         else ());
                    iter tail)
               | [] -> () in
             (iter field_yojsons;
              (match Ppx_yojson_conv_lib.(!) duplicates with
               | _::_ ->
                   Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields
                     _tp_loc (Ppx_yojson_conv_lib.(!) duplicates) yojson
               | [] ->
                   (match Ppx_yojson_conv_lib.(!) extra with
                    | _::_ ->
                        Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields
                          _tp_loc (Ppx_yojson_conv_lib.(!) extra) yojson
                    | [] ->
                        (match ((Ppx_yojson_conv_lib.(!) x_field),
                                 (Ppx_yojson_conv_lib.(!) y_field))
                         with
                         | (Ppx_yojson_conv_lib.Option.Some x_value,
                            Ppx_yojson_conv_lib.Option.Some y_value) ->
                             { x = x_value; y = y_value }
                         | _ ->
                             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                               _tp_loc yojson
                               [((Ppx_yojson_conv_lib.poly_equal
                                    (Ppx_yojson_conv_lib.(!) x_field)
                                    Ppx_yojson_conv_lib.Option.None), \"x\");
                               ((Ppx_yojson_conv_lib.poly_equal
                                   (Ppx_yojson_conv_lib.(!) y_field)
                                   Ppx_yojson_conv_lib.Option.None), \"y\")]))))
         | _ as yojson ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom
               _tp_loc yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> point)
      let _ = point_of_yojson
      let yojson_of_point =
        (function
         | { x = v_x; y = v_y } ->
             let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
             let bnds = let arg = yojson_of_float v_y in (\"y\", arg) :: bnds in
             let bnds = let arg = yojson_of_float v_x in (\"x\", arg) :: bnds in
             `Assoc bnds : point -> Ppx_yojson_conv_lib.Yojson.Safe.t)
      let _ = yojson_of_point
    end[@@ocaml.doc \"@inline\"][@@merlin.hide ]
   ]",
    "notifications": []
  }
 
  $ cat >dune-project <<EOF
  > (lang dune 2.9)
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name apc)
  >  (libraries ppx_compare)
  >  (preprocess (pps ppx_compare)))
  > EOF

  $ cat > apc.ml << EOF
  > open Base
  > type t = {v:int ; w:int} [@@deriving compare]
  > EOF

  $ dune build

  $ $MERLIN single expand-node -position 2:31 -filename ./apc.ml < ./apc.ml
  Parsetree nodes: [ core_type; label_declaration; type_kind; type_declaration; structure_item; structure ]
  Typedtree nodes: [ core_type; core_type; label_declaration; type_kind; type_declaration; structure_item; structure ]
  {
    "class": "return",
    "value": "[ type t = {
    v: int ;
    w: int }[@@deriving compare]
  ; include
    struct
      let _ = fun (_ : t) -> ()
      let compare =
        (fun a__001_ b__002_ ->
           if Stdlib.(==) a__001_ b__002_
           then 0
           else
             (match compare_int a__001_.v b__002_.v with
              | 0 -> compare_int a__001_.w b__002_.w
              | n -> n) : t -> ((t)[@merlin.hide ]) -> int)
      let _ = compare
    end[@@ocaml.doc \"@inline\"][@@merlin.hide ]
   ]",
    "notifications": []
  }
