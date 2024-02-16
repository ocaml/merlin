# Recovery in structures

  $ cat >test.ml <<EOF
  > type t = A | B
  > let f (x : t) =
  >   match x with
  >   | A -> ()
  >   | B -> 3
  >   | C -> 'a'
  > EOF

  $ $MERLIN single errors -filename test.ml < test.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 5,
          "col": 9
        },
        "end": {
          "line": 5,
          "col": 10
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type int but an expression was expected of type unit"
      },
      {
        "start": {
          "line": 6,
          "col": 4
        },
        "end": {
          "line": 6,
          "col": 5
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This variant pattern is expected to have type t
  There is no constructor C within type t"
      },
      {
        "start": {
          "line": 6,
          "col": 9
        },
        "end": {
          "line": 6,
          "col": 12
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type char but an expression was expected of type unit"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single dump -what typedtree -filename test.ml < test.ml
  {
    "class": "return",
    "value": "[
    structure_item (test.ml[1,0+0]..test.ml[1,0+14])
      Tstr_type Rec
      [
        type_declaration t/276 (test.ml[1,0+0]..test.ml[1,0+14])
          ptype_params =
            []
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_variant
              [
                (test.ml[1,0+9]..test.ml[1,0+10])
                  A/277
                  []
                  None
                (test.ml[1,0+11]..test.ml[1,0+14])
                  B/278
                  []
                  None
              ]
          ptype_private = Public
          ptype_manifest =
            None
      ]
    structure_item (test.ml[2,15+0]..test.ml[6,69+12])
      Tstr_value Nonrec
      [
        <def>
          pattern (test.ml[2,15+4]..test.ml[2,15+5])
            Tpat_var \"f/279\"
          expression (test.ml[2,15+6]..test.ml[6,69+12]) ghost
            Texp_function
            [
              Nolabel
              Param_pat
                pattern (test.ml[2,15+7]..test.ml[2,15+8])
                  extra
                    Tpat_extra_constraint
                    core_type (test.ml[2,15+11]..test.ml[2,15+12])
                      Ttyp_constr \"t/276\"
                      []
                  Tpat_alias \"x/281\"
                  pattern (test.ml[2,15+7]..test.ml[2,15+8]) ghost
                    attribute \"merlin.hide\"
                      []
                    Tpat_any
            ]
            Tfunction_body
              expression (test.ml[3,31+2]..test.ml[6,69+12])
                Texp_match
                expression (test.ml[3,31+8]..test.ml[3,31+9])
                  Texp_ident \"x/281\"
                [
                  <case>
                    pattern (test.ml[4,46+4]..test.ml[4,46+5])
                      Tpat_value
                      pattern (test.ml[4,46+4]..test.ml[4,46+5])
                        Tpat_construct \"A\"
                        []
                        None
                    expression (test.ml[4,46+9]..test.ml[4,46+11])
                      attribute \"merlin.loc\"
                        []
                      Texp_construct \"()\"
                      []
                  <case>
                    pattern (test.ml[5,58+4]..test.ml[5,58+5])
                      Tpat_value
                      pattern (test.ml[5,58+4]..test.ml[5,58+5])
                        Tpat_construct \"B\"
                        []
                        None
                    expression (test.ml[5,58+9]..test.ml[5,58+10])
                      attribute \"merlin.incorrect\"
                        []
                      attribute \"merlin.saved-parts\"
                        [
                          structure_item (_none_[0,0+-1]..[0,0+-1]) ghost
                            Pstr_eval
                            expression (_none_[0,0+-1]..[0,0+-1]) ghost
                              Pexp_constant PConst_int (1,None)
                        ]
                      attribute \"merlin.loc\"
                        []
                      Texp_ident \"*type-error*/282\"
                  <case>
                    pattern (test.ml[6,69+4]..test.ml[6,69+5])
                      Tpat_value
                      pattern (test.ml[6,69+4]..test.ml[6,69+5])
                        attribute \"merlin.incorrect\"
                          []
                        Tpat_any
                    expression (test.ml[6,69+9]..test.ml[6,69+12])
                      attribute \"merlin.incorrect\"
                        []
                      attribute \"merlin.saved-parts\"
                        [
                          structure_item (_none_[0,0+-1]..[0,0+-1]) ghost
                            Pstr_eval
                            expression (_none_[0,0+-1]..[0,0+-1]) ghost
                              Pexp_constant PConst_int (2,None)
                        ]
                      attribute \"merlin.loc\"
                        []
                      Texp_ident \"*type-error*/283\"
                ]
      ]
  ]
  
  
  ",
    "notifications": []
  }

  $ cat >test2.ml <<EOF
  > type t = A | B
  > let f (C : t) : int = ()
  > EOF

  $ $MERLIN single errors -filename test2.ml < test2.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 7
        },
        "end": {
          "line": 2,
          "col": 8
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This variant pattern is expected to have type t
  There is no constructor C within type t"
      },
      {
        "start": {
          "line": 2,
          "col": 22
        },
        "end": {
          "line": 2,
          "col": 24
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type unit but an expression was expected of type int"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single dump -what typedtree -filename test2.ml < test2.ml
  {
    "class": "return",
    "value": "[
    structure_item (test2.ml[1,0+0]..test2.ml[1,0+14])
      Tstr_type Rec
      [
        type_declaration t/276 (test2.ml[1,0+0]..test2.ml[1,0+14])
          ptype_params =
            []
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_variant
              [
                (test2.ml[1,0+9]..test2.ml[1,0+10])
                  A/277
                  []
                  None
                (test2.ml[1,0+11]..test2.ml[1,0+14])
                  B/278
                  []
                  None
              ]
          ptype_private = Public
          ptype_manifest =
            None
      ]
    structure_item (test2.ml[2,15+0]..test2.ml[2,15+24])
      Tstr_value Nonrec
      [
        <def>
          pattern (test2.ml[2,15+4]..test2.ml[2,15+5])
            Tpat_var \"f/279\"
          expression (test2.ml[2,15+6]..test2.ml[2,15+24]) ghost
            Texp_function
            [
              Nolabel
              Param_pat
                pattern (test2.ml[2,15+7]..test2.ml[2,15+8])
                  attribute \"merlin.incorrect\"
                    []
                  extra
                    Tpat_extra_constraint
                    core_type (test2.ml[2,15+11]..test2.ml[2,15+12])
                      Ttyp_constr \"t/276\"
                      []
                  Tpat_any
            ]
            Tfunction_body
              expression (test2.ml[2,15+22]..test2.ml[2,15+24])
                attribute \"merlin.incorrect\"
                  []
                attribute \"merlin.saved-parts\"
                  [
                    structure_item (_none_[0,0+-1]..[0,0+-1]) ghost
                      Pstr_eval
                      expression (_none_[0,0+-1]..[0,0+-1]) ghost
                        Pexp_constant PConst_int (1,None)
                  ]
                extra
                  Texp_constraint
                  core_type (test2.ml[2,15+16]..test2.ml[2,15+19])
                    Ttyp_constr \"int/1!\"
                    []
                Texp_ident \"*type-error*/281\"
      ]
  ]
  
  
  ",
    "notifications": []
  }

# Recovery in signatures

First a simple case:

  $ cat >test.mli <<EOF
  > val foo1 : int
  > 
  > val foo2 : int * toto
  > 
  > val foo3 : int * char
  > EOF

  $ $MERLIN single errors -filename test.mli < test.mli
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 17
        },
        "end": {
          "line": 3,
          "col": 21
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound type constructor toto"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single dump -what typedtree -filename test.mli < test.mli
  {
    "class": "return",
    "value": "[
    signature_item (test.mli[1,0+0]..test.mli[1,0+14])
      Tsig_value
      value_description foo1/276 (test.mli[1,0+0]..test.mli[1,0+14])
        core_type (test.mli[1,0+11]..test.mli[1,0+14])
          Ttyp_constr \"int/1!\"
          []
        []
    signature_item (test.mli[3,16+0]..test.mli[3,16+21])
      Tsig_value
      value_description foo2/277 (test.mli[3,16+0]..test.mli[3,16+21])
        core_type (test.mli[3,16+11]..test.mli[3,16+21])
          Ttyp_tuple
          [
            core_type (test.mli[3,16+11]..test.mli[3,16+14])
              Ttyp_constr \"int/1!\"
              []
            core_type (test.mli[3,16+17]..test.mli[3,16+21])
              Ttyp_any
          ]
        []
    signature_item (test.mli[5,39+0]..test.mli[5,39+21])
      Tsig_value
      value_description foo3/278 (test.mli[5,39+0]..test.mli[5,39+21])
        core_type (test.mli[5,39+11]..test.mli[5,39+21])
          Ttyp_tuple
          [
            core_type (test.mli[5,39+11]..test.mli[5,39+14])
              Ttyp_constr \"int/1!\"
              []
            core_type (test.mli[5,39+17]..test.mli[5,39+21])
              Ttyp_constr \"char/2!\"
              []
          ]
        []
  ]
  
  
  ",
    "notifications": []
  }

And now, with an error deep in a submodule:

  $ cat >test2.mli <<EOF
  > val foo1 : int
  > 
  > module M : sig
  >   val foo21 : int
  >   module N : sig
  >     val foo211 : int
  >     val foo212 : int * toto
  >     val foo213 : int * char
  >   end
  > end
  > 
  > val foo3 : int * char
  > EOF

  $ $MERLIN single errors -filename test2.mli < test2.mli
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 7,
          "col": 23
        },
        "end": {
          "line": 7,
          "col": 27
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound type constructor toto"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single dump -what typedtree -filename test2.mli < test2.mli
  {
    "class": "return",
    "value": "[
    signature_item (test2.mli[1,0+0]..test2.mli[1,0+14])
      Tsig_value
      value_description foo1/276 (test2.mli[1,0+0]..test2.mli[1,0+14])
        core_type (test2.mli[1,0+11]..test2.mli[1,0+14])
          Ttyp_constr \"int/1!\"
          []
        []
    signature_item (test2.mli[3,16+0]..test2.mli[10,149+3])
      Tsig_module \"M/282\"
      module_type (test2.mli[3,16+11]..test2.mli[10,149+3])
        Tmty_signature
        [
          signature_item (test2.mli[4,31+2]..test2.mli[4,31+17])
            Tsig_value
            value_description foo21/277 (test2.mli[4,31+2]..test2.mli[4,31+17])
              core_type (test2.mli[4,31+14]..test2.mli[4,31+17])
                Ttyp_constr \"int/1!\"
                []
              []
          signature_item (test2.mli[5,49+2]..test2.mli[9,143+5])
            Tsig_module \"N/281\"
            module_type (test2.mli[5,49+13]..test2.mli[9,143+5])
              Tmty_signature
              [
                signature_item (test2.mli[6,66+4]..test2.mli[6,66+20])
                  Tsig_value
                  value_description foo211/278 (test2.mli[6,66+4]..test2.mli[6,66+20])
                    core_type (test2.mli[6,66+17]..test2.mli[6,66+20])
                      Ttyp_constr \"int/1!\"
                      []
                    []
                signature_item (test2.mli[7,87+4]..test2.mli[7,87+27])
                  Tsig_value
                  value_description foo212/279 (test2.mli[7,87+4]..test2.mli[7,87+27])
                    core_type (test2.mli[7,87+17]..test2.mli[7,87+27])
                      Ttyp_tuple
                      [
                        core_type (test2.mli[7,87+17]..test2.mli[7,87+20])
                          Ttyp_constr \"int/1!\"
                          []
                        core_type (test2.mli[7,87+23]..test2.mli[7,87+27])
                          Ttyp_any
                      ]
                    []
                signature_item (test2.mli[8,115+4]..test2.mli[8,115+27])
                  Tsig_value
                  value_description foo213/280 (test2.mli[8,115+4]..test2.mli[8,115+27])
                    core_type (test2.mli[8,115+17]..test2.mli[8,115+27])
                      Ttyp_tuple
                      [
                        core_type (test2.mli[8,115+17]..test2.mli[8,115+20])
                          Ttyp_constr \"int/1!\"
                          []
                        core_type (test2.mli[8,115+23]..test2.mli[8,115+27])
                          Ttyp_constr \"char/2!\"
                          []
                      ]
                    []
              ]
        ]
    signature_item (test2.mli[12,154+0]..test2.mli[12,154+21])
      Tsig_value
      value_description foo3/283 (test2.mli[12,154+0]..test2.mli[12,154+21])
        core_type (test2.mli[12,154+11]..test2.mli[12,154+21])
          Ttyp_tuple
          [
            core_type (test2.mli[12,154+11]..test2.mli[12,154+14])
              Ttyp_constr \"int/1!\"
              []
            core_type (test2.mli[12,154+17]..test2.mli[12,154+21])
              Ttyp_constr \"char/2!\"
              []
          ]
        []
  ]
  
  
  ",
    "notifications": []
  }

# Recovery for core types

Actually the most likely error for signatures is an error in a core type, let's
make sure we also handle that correctly in structures:

  $ cat >test_ct.ml <<EOF
  > let foo1 : int = 3
  > 
  > let foo2 : int * toto = 3, 4
  > 
  > let foo3 : int * int = 3, 4
  > EOF

  $ $MERLIN single errors -filename test_ct.ml < test_ct.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 17
        },
        "end": {
          "line": 3,
          "col": 21
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound type constructor toto"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single dump -what typedtree -filename test_ct.ml < test_ct.ml
  {
    "class": "return",
    "value": "[
    structure_item (test_ct.ml[1,0+0]..test_ct.ml[1,0+18])
      Tstr_value Nonrec
      [
        <def>
          pattern (test_ct.ml[1,0+4]..test_ct.ml[1,0+8])
            extra
              Tpat_extra_constraint
              core_type (test_ct.ml[1,0+11]..test_ct.ml[1,0+14])
                Ttyp_constr \"int/1!\"
                []
            Tpat_alias \"foo1/276\"
            pattern (test_ct.ml[1,0+4]..test_ct.ml[1,0+8]) ghost
              attribute \"merlin.hide\"
                []
              Tpat_any
          expression (test_ct.ml[1,0+17]..test_ct.ml[1,0+18])
            extra
              Texp_constraint
              core_type (test_ct.ml[1,0+11]..test_ct.ml[1,0+14])
                Ttyp_constr \"int/1!\"
                []
            Texp_constant Const_int 3
      ]
    structure_item (test_ct.ml[3,20+0]..test_ct.ml[3,20+28])
      Tstr_value Nonrec
      [
        <def>
          pattern (test_ct.ml[3,20+4]..test_ct.ml[3,20+8])
            extra
              Tpat_extra_constraint
              core_type (test_ct.ml[3,20+11]..test_ct.ml[3,20+21])
                Ttyp_tuple
                [
                  core_type (test_ct.ml[3,20+11]..test_ct.ml[3,20+14])
                    Ttyp_constr \"int/1!\"
                    []
                  core_type (test_ct.ml[3,20+17]..test_ct.ml[3,20+21])
                    Ttyp_any
                ]
            Tpat_alias \"foo2/277\"
            pattern (test_ct.ml[3,20+4]..test_ct.ml[3,20+8]) ghost
              attribute \"merlin.hide\"
                []
              Tpat_any
          expression (test_ct.ml[3,20+24]..test_ct.ml[3,20+28])
            extra
              Texp_constraint
              core_type (test_ct.ml[3,20+11]..test_ct.ml[3,20+21])
                Ttyp_tuple
                [
                  core_type (test_ct.ml[3,20+11]..test_ct.ml[3,20+14])
                    Ttyp_constr \"int/1!\"
                    []
                  core_type (test_ct.ml[3,20+17]..test_ct.ml[3,20+21])
                    Ttyp_any
                ]
            Texp_tuple
            [
              expression (test_ct.ml[3,20+24]..test_ct.ml[3,20+25])
                Texp_constant Const_int 3
              expression (test_ct.ml[3,20+27]..test_ct.ml[3,20+28])
                Texp_constant Const_int 4
            ]
      ]
    structure_item (test_ct.ml[5,50+0]..test_ct.ml[5,50+27])
      Tstr_value Nonrec
      [
        <def>
          pattern (test_ct.ml[5,50+4]..test_ct.ml[5,50+8])
            extra
              Tpat_extra_constraint
              core_type (test_ct.ml[5,50+11]..test_ct.ml[5,50+20])
                Ttyp_tuple
                [
                  core_type (test_ct.ml[5,50+11]..test_ct.ml[5,50+14])
                    Ttyp_constr \"int/1!\"
                    []
                  core_type (test_ct.ml[5,50+17]..test_ct.ml[5,50+20])
                    Ttyp_constr \"int/1!\"
                    []
                ]
            Tpat_alias \"foo3/278\"
            pattern (test_ct.ml[5,50+4]..test_ct.ml[5,50+8]) ghost
              attribute \"merlin.hide\"
                []
              Tpat_any
          expression (test_ct.ml[5,50+23]..test_ct.ml[5,50+27])
            extra
              Texp_constraint
              core_type (test_ct.ml[5,50+11]..test_ct.ml[5,50+20])
                Ttyp_tuple
                [
                  core_type (test_ct.ml[5,50+11]..test_ct.ml[5,50+14])
                    Ttyp_constr \"int/1!\"
                    []
                  core_type (test_ct.ml[5,50+17]..test_ct.ml[5,50+20])
                    Ttyp_constr \"int/1!\"
                    []
                ]
            Texp_tuple
            [
              expression (test_ct.ml[5,50+23]..test_ct.ml[5,50+24])
                Texp_constant Const_int 3
              expression (test_ct.ml[5,50+26]..test_ct.ml[5,50+27])
                Texp_constant Const_int 4
            ]
      ]
  ]
  
  
  ",
    "notifications": []
  }

# Spurious errors

Sometimes typing recovery consists in dropping intermediate nodes, which could
generate errors in subtrees. We don't want to tell the user about sub errors,
since they might go away when fixing the first one.

FIXME

  $ cat >open.ml <<EOF
  > let x = false
  > let f () =
  >   let open Unknown in
  >   x
  > ;;
  > 
  > let g () =
  >   let open Lsit in
  >   map
  > ;;
  > EOF

  $ $MERLIN single errors -filename open.ml < open.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 11
        },
        "end": {
          "line": 3,
          "col": 18
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound module Unknown"
      },
      {
        "start": {
          "line": 8,
          "col": 11
        },
        "end": {
          "line": 8,
          "col": 15
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound module Lsit
  Hint: Did you mean List?"
      }
    ],
    "notifications": []
  }
