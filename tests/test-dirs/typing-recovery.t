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
  The constructor C does not belong to type t"
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
                  attribute \"merlin.incorrect\"
                    []
                  attribute \"merlin.saved-parts\"
                    [
                      structure_item (_none_[0,0+-1]..[0,0+-1]) ghost
                        Pstr_eval
                        expression (_none_[0,0+-1]..[0,0+-1]) ghost
                          Pexp_constant PConst_int (1,None)
                    ]
                  Texp_ident \"*type-error*/87\"
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
  The constructor C does not belong to type t"
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
            attribute \"merlin.incorrect\"
              []
            Texp_ident \"*type-error*/86\"
      ]
  ]
  
  
  ",
    "notifications": []
  }
