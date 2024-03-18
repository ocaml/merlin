Tests with ppx_yojson_conv
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
  > type t = {x:float; y:float} [@@deriving yojson]
  > type z = {a:float; b:float;} [@@deriving yojson]
  > EOF

  $ dune build

cursor is on "deriving"
type t = {x:float; y:float} without a semicolon after the y field
expand-node has core-type as a leaf node when a semicolon is not added
  $ $MERLIN single expand-node -position 2:34 -filename ./apx.ml < ./apx.ml
  Parsetree nodes: [ core_type; label_declaration; type_kind; type_declaration; structure_item; structure ]
  Typedtree nodes: [ core_type; core_type; label_declaration; type_kind; type_declaration; structure_item; structure ]
  {
    "class": "return",
    "value": "[ type t = {
    x: float ;
    y: float }[@@deriving yojson]
  ; include
    struct
      let _ = fun (_ : t) -> ()
      let t_of_yojson =
        (let _tp_loc = \"apx.ml.t\" in
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
               _tp_loc yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)
      let _ = t_of_yojson
      let yojson_of_t =
        (function
         | { x = v_x; y = v_y } ->
             let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
             let bnds = let arg = yojson_of_float v_y in (\"y\", arg) :: bnds in
             let bnds = let arg = yojson_of_float v_x in (\"x\", arg) :: bnds in
             `Assoc bnds : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)
      let _ = yojson_of_t
    end[@@ocaml.doc \"@inline\"][@@merlin.hide ]
   ]",
    "notifications": []
  }

cursor is on "deriving"
type t = {x:float; y:float;} with a semicolon after the y field
expand-node has label-declaration as a leaf node when the semicolon is added.
  $ $MERLIN single expand-node -position 3:34 -filename ./apx.ml < ./apx.ml
  Parsetree nodes: [ label_declaration; type_kind; type_declaration; structure_item; structure ]
  Typedtree nodes: [ label_declaration; type_kind; type_declaration; structure_item; structure ]
  {
    "class": "return",
    "value": "[ type z = {
    a: float ;
    b: float }[@@deriving yojson]
  ; include
    struct
      let _ = fun (_ : z) -> ()
      let z_of_yojson =
        (let _tp_loc = \"apx.ml.z\" in
         function
         | `Assoc field_yojsons as yojson ->
             let a_field = ref Ppx_yojson_conv_lib.Option.None
             and b_field = ref Ppx_yojson_conv_lib.Option.None
             and duplicates = ref []
             and extra = ref [] in
             let rec iter =
               function
               | (field_name, _field_yojson)::tail ->
                   ((match field_name with
                     | \"a\" ->
                         (match Ppx_yojson_conv_lib.(!) a_field with
                          | Ppx_yojson_conv_lib.Option.None ->
                              let fvalue = float_of_yojson _field_yojson in
                              a_field :=
                                (Ppx_yojson_conv_lib.Option.Some fvalue)
                          | Ppx_yojson_conv_lib.Option.Some _ ->
                              duplicates := (field_name ::
                                (Ppx_yojson_conv_lib.(!) duplicates)))
                     | \"b\" ->
                         (match Ppx_yojson_conv_lib.(!) b_field with
                          | Ppx_yojson_conv_lib.Option.None ->
                              let fvalue = float_of_yojson _field_yojson in
                              b_field :=
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
                        (match ((Ppx_yojson_conv_lib.(!) a_field),
                                 (Ppx_yojson_conv_lib.(!) b_field))
                         with
                         | (Ppx_yojson_conv_lib.Option.Some a_value,
                            Ppx_yojson_conv_lib.Option.Some b_value) ->
                             { a = a_value; b = b_value }
                         | _ ->
                             Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                               _tp_loc yojson
                               [((Ppx_yojson_conv_lib.poly_equal
                                    (Ppx_yojson_conv_lib.(!) a_field)
                                    Ppx_yojson_conv_lib.Option.None), \"a\");
                               ((Ppx_yojson_conv_lib.poly_equal
                                   (Ppx_yojson_conv_lib.(!) b_field)
                                   Ppx_yojson_conv_lib.Option.None), \"b\")]))))
         | _ as yojson ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom
               _tp_loc yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> z)
      let _ = z_of_yojson
      let yojson_of_z =
        (function
         | { a = v_a; b = v_b } ->
             let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
             let bnds = let arg = yojson_of_float v_b in (\"b\", arg) :: bnds in
             let bnds = let arg = yojson_of_float v_a in (\"a\", arg) :: bnds in
             `Assoc bnds : z -> Ppx_yojson_conv_lib.Yojson.Safe.t)
      let _ = yojson_of_z
    end[@@ocaml.doc \"@inline\"][@@merlin.hide ]
   ]",
    "notifications": []
  }


Tests with ppx_deriving_yojson
  $ cat >dune-project <<EOF
  > (lang dune 2.9)
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name apt)
  >  (libraries yojson)
  >  (preprocess (pps ppx_deriving_yojson)))
  > EOF

  $ cat > apt.ml << EOF
  > type t = {x:float; y:float} [@@deriving yojson]
  > type z = {x:float; y:float;} [@@deriving yojson]
  > EOF

  $ dune build

cursor is on "deriving"
type t = {x:float; y:float} without a semicolon after the y field
expand-node has expression as a leaf node when a semicolon is not added
  $ $MERLIN single expand-node -position 1:34 -filename ./apt.ml < ./apt.ml
  Parsetree nodes: [ expression; value_binding; structure_item; structure ]
  Typedtree nodes: [ expression; value_binding; structure_item; structure ]
  {
    "class": "return",
    "value": "No deriver on this node",
    "notifications": []
  }

cursor is on "deriving"
type t = {x:float; y:float} with a semicolon after the y field
expand-node has expression as a leaf node when the semicolon is added.
  $ $MERLIN single expand-node -position 2:35 -filename ./apt.ml < ./apt.ml
  Parsetree nodes: [ expression; value_binding; structure_item; structure ]
  Typedtree nodes: [ expression; value_binding; structure_item; structure ]
  {
    "class": "return",
    "value": "No deriver on this node",
    "notifications": []
  }
