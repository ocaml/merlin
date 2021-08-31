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
        type_declaration t/81 (test.ml[1,0+0]..test.ml[1,0+14])
          ptype_params =
            []
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_variant
              [
                (test.ml[1,0+9]..test.ml[1,0+10])
                  A/82
                  []
                  None
                (test.ml[1,0+11]..test.ml[1,0+14])
                  B/83
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
            Tpat_var \"f/84\"
          expression (test.ml[2,15+6]..test.ml[6,69+12]) ghost
            Texp_function
            Nolabel
            [
              <case>
                pattern (test.ml[2,15+7]..test.ml[2,15+8])
                  Tpat_extra_constraint
                  core_type (test.ml[2,15+11]..test.ml[2,15+12])
                    Ttyp_constr \"t/81\"
                    []
                  pattern (test.ml[2,15+7]..test.ml[2,15+8])
                    Tpat_alias \"x/86\"
                    pattern (test.ml[2,15+7]..test.ml[2,15+8])
                      Tpat_any
                expression (test.ml[3,31+2]..test.ml[6,69+12])
                  Texp_match
                  expression (test.ml[3,31+8]..test.ml[3,31+9])
                    Texp_ident \"x/86\"
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
                                Pexp_constant PConst_int (2,None)
                          ]
                        Texp_ident \"*type-error*/87\"
                    <case>
                      pattern (test.ml[6,69+4]..test.ml[6,69+5])
                        Tpat_value
                        pattern (test.ml[6,69+4]..test.ml[6,69+5])
                          attribute \"merlin.incorrect\"
                            []
                          attribute \"merlin.saved-parts\"
                            [
                              structure_item (_none_[0,0+-1]..[0,0+-1]) ghost
                                Pstr_eval
                                expression (_none_[0,0+-1]..[0,0+-1]) ghost
                                  Pexp_constant PConst_int (1,None)
                            ]
                          Tpat_any
                      expression (test.ml[6,69+9]..test.ml[6,69+12])
                        attribute \"merlin.incorrect\"
                          []
                        attribute \"merlin.saved-parts\"
                          [
                            structure_item (_none_[0,0+-1]..[0,0+-1]) ghost
                              Pstr_eval
                              expression (_none_[0,0+-1]..[0,0+-1]) ghost
                                Pexp_constant PConst_int (3,None)
                          ]
                        Texp_ident \"*type-error*/88\"
                  ]
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
        type_declaration t/81 (test2.ml[1,0+0]..test2.ml[1,0+14])
          ptype_params =
            []
          ptype_cstrs =
            []
          ptype_kind =
            Ttype_variant
              [
                (test2.ml[1,0+9]..test2.ml[1,0+10])
                  A/82
                  []
                  None
                (test2.ml[1,0+11]..test2.ml[1,0+14])
                  B/83
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
            Tpat_var \"f/84\"
          expression (test2.ml[2,15+6]..test2.ml[2,15+24]) ghost
            Texp_function
            Nolabel
            [
              <case>
                pattern (test2.ml[2,15+7]..test2.ml[2,15+8])
                  attribute \"merlin.incorrect\"
                    []
                  Tpat_extra_constraint
                  core_type (test2.ml[2,15+11]..test2.ml[2,15+12])
                    Ttyp_constr \"t/81\"
                    []
                  pattern (test2.ml[2,15+7]..test2.ml[2,15+8])
                    attribute \"merlin.incorrect\"
                      []
                    Tpat_any
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
                  Texp_constraint
                  core_type (test2.ml[2,15+16]..test2.ml[2,15+19])
                    Ttyp_constr \"int/1!\"
                    []
                    Texp_ident \"*type-error*/86\"
            ]
      ]
  ]
  
  
  ",
    "notifications": []
  }
