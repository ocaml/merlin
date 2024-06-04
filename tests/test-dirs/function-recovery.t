  $ cat >test.ml <<'EOF'
  > module ERROR_locate_from_inside_function_literal_used_as_non_function = struct
  >   let problem = `Problem
  >   let () = fun () -> problem
  > EOF

  $ $MERLIN single dump -what typedtree -filename test.ml < test.ml 
  {
    "class": "return",
    "value": "[
    structure_item (test.ml[1,0+0]..test.ml[3,104+28])
      Tstr_module
      ERROR_locate_from_inside_function_literal_used_as_non_function/279
        module_expr (test.ml[1,0+72]..test.ml[3,104+28])
          Tmod_structure
          [
            structure_item (test.ml[2,79+2]..test.ml[2,79+24])
              Tstr_value Nonrec
              [
                <def>
                  pattern (test.ml[2,79+6]..test.ml[2,79+13])
                    Tpat_var \"problem/276\"
                  expression (test.ml[2,79+16]..test.ml[2,79+24])
                    Texp_variant \"Problem\"
                    None
              ]
            structure_item (test.ml[3,104+2]..test.ml[3,104+28])
              Tstr_value Nonrec
              [
                <def>
                  pattern (test.ml[3,104+6]..test.ml[3,104+8])
                    Tpat_construct \"()\"
                    []
                    None
                  expression (test.ml[3,104+11]..test.ml[3,104+28])
                    Texp_function
                    [
                      Nolabel
                      Param_pat
                        pattern (test.ml[3,104+15]..test.ml[3,104+17])
                          Tpat_construct \"()\"
                          []
                          None
                    ]
                    Tfunction_body
                      expression (test.ml[3,104+21]..test.ml[3,104+28])
                        attribute \"merlin.incorrect\"
                          []
                        attribute \"merlin.saved-parts\"
                          [
                            structure_item (_none_[0,0+-1]..[0,0+-1]) ghost
                              Pstr_eval
                              expression (_none_[0,0+-1]..[0,0+-1]) ghost
                                Pexp_constant PConst_int (1,None)
                          ]
                        Texp_ident \"*type-error*/277\"
              ]
          ]
  ]
  
  
  ",
    "notifications": []
  }

  $ $MERLIN single dump -what typedtree -filename type.ml <<EOF
  > let f = fun (type t) (foo : t list) -> let (_ : t) = () in ()
  > EOF
  {
    "class": "return",
    "value": "[
    structure_item (type.ml[1,0+0]..type.ml[1,0+61])
      Tstr_value Nonrec
      [
        <def>
          pattern (type.ml[1,0+4]..type.ml[1,0+5])
            Tpat_var \"f/276\"
          expression (type.ml[1,0+8]..type.ml[1,0+61])
            extra
              Texp_newtype' \"t/278\"
            Texp_function
            [
              Nolabel
              Param_pat
                pattern (type.ml[1,0+22]..type.ml[1,0+25])
                  extra
                    Tpat_extra_constraint
                    core_type (type.ml[1,0+28]..type.ml[1,0+34])
                      Ttyp_constr \"list/9!\"
                      [
                        core_type (type.ml[1,0+28]..type.ml[1,0+29])
                          Ttyp_constr \"t/278\"
                          []
                      ]
                  Tpat_alias \"foo/279\"
                  pattern (type.ml[1,0+22]..type.ml[1,0+25]) ghost
                    attribute \"merlin.hide\"
                      []
                    Tpat_any
            ]
            Tfunction_body
              expression (type.ml[1,0+39]..type.ml[1,0+61])
                Texp_let Nonrec
                [
                  <def>
                    pattern (type.ml[1,0+44]..type.ml[1,0+45])
                      extra
                        Tpat_extra_constraint
                        core_type (type.ml[1,0+48]..type.ml[1,0+49])
                          Ttyp_constr \"t/278\"
                          []
                      Tpat_any
                    expression (type.ml[1,0+53]..type.ml[1,0+55])
                      attribute \"merlin.incorrect\"
                        []
                      attribute \"merlin.saved-parts\"
                        [
                          structure_item (_none_[0,0+-1]..[0,0+-1]) ghost
                            Pstr_eval
                            expression (_none_[0,0+-1]..[0,0+-1]) ghost
                              Pexp_constant PConst_int (1,None)
                        ]
                      Texp_ident \"*type-error*/280\"
                ]
                expression (type.ml[1,0+59]..type.ml[1,0+61])
                  attribute \"merlin.loc\"
                    []
                  Texp_construct \"()\"
                  []
      ]
  ]
  
  
  ",
    "notifications": []
  }

  $ $MERLIN single dump -what browse -filename test.ml <<'EOF'
  > let f (x) : int = function
  >   | None -> 3
  >   | Some 5 -> 4
  >   | Some _aa -> 4
  > EOF
  {
    "class": "return",
    "value": [
      {
        "filename": "test.ml",
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 4,
          "col": 17
        },
        "ghost": false,
        "attrs": [],
        "kind": "structure",
        "children": [
          {
            "filename": "test.ml",
            "start": {
              "line": 1,
              "col": 0
            },
            "end": {
              "line": 4,
              "col": 17
            },
            "ghost": false,
            "attrs": [],
            "kind": "structure_item",
            "children": [
              {
                "filename": "test.ml",
                "start": {
                  "line": 1,
                  "col": 0
                },
                "end": {
                  "line": 4,
                  "col": 17
                },
                "ghost": false,
                "attrs": [],
                "kind": "value_binding",
                "children": [
                  {
                    "filename": "test.ml",
                    "start": {
                      "line": 1,
                      "col": 4
                    },
                    "end": {
                      "line": 1,
                      "col": 5
                    },
                    "ghost": false,
                    "attrs": [],
                    "kind": "pattern (test.ml[1,0+4]..test.ml[1,0+5])
    Tpat_var \"f/276\"
  ",
                    "children": []
                  },
                  {
                    "filename": "test.ml",
                    "start": {
                      "line": 1,
                      "col": 6
                    },
                    "end": {
                      "line": 4,
                      "col": 17
                    },
                    "ghost": true,
                    "attrs": [],
                    "kind": "expression",
                    "children": [
                      {
                        "filename": "test.ml",
                        "start": {
                          "line": 1,
                          "col": 6
                        },
                        "end": {
                          "line": 1,
                          "col": 9
                        },
                        "ghost": false,
                        "attrs": [],
                        "kind": "pattern (test.ml[1,0+6]..test.ml[1,0+9])
    Tpat_var \"x/278\"
  ",
                        "children": []
                      },
                      {
                        "filename": "test.ml",
                        "start": {
                          "line": 2,
                          "col": 4
                        },
                        "end": {
                          "line": 2,
                          "col": 13
                        },
                        "ghost": false,
                        "attrs": [],
                        "kind": "case",
                        "children": [
                          {
                            "filename": "test.ml",
                            "start": {
                              "line": 2,
                              "col": 4
                            },
                            "end": {
                              "line": 2,
                              "col": 8
                            },
                            "ghost": false,
                            "attrs": [],
                            "kind": "pattern (test.ml[2,27+4]..test.ml[2,27+8])
    Tpat_construct \"None\"
    []
    None
  ",
                            "children": []
                          },
                          {
                            "filename": "test.ml",
                            "start": {
                              "line": 2,
                              "col": 12
                            },
                            "end": {
                              "line": 2,
                              "col": 13
                            },
                            "ghost": false,
                            "attrs": [
                              {
                                "start": {
                                  "line": 2,
                                  "col": 11
                                },
                                "end": {
                                  "line": 2,
                                  "col": 13
                                },
                                "name": "merlin.loc"
                              }
                            ],
                            "kind": "expression",
                            "children": []
                          }
                        ]
                      },
                      {
                        "filename": "test.ml",
                        "start": {
                          "line": 3,
                          "col": 4
                        },
                        "end": {
                          "line": 3,
                          "col": 15
                        },
                        "ghost": false,
                        "attrs": [],
                        "kind": "case",
                        "children": [
                          {
                            "filename": "test.ml",
                            "start": {
                              "line": 3,
                              "col": 4
                            },
                            "end": {
                              "line": 3,
                              "col": 10
                            },
                            "ghost": false,
                            "attrs": [],
                            "kind": "pattern (test.ml[3,41+4]..test.ml[3,41+10])
    Tpat_construct \"Some\"
    [
      pattern (test.ml[3,41+9]..test.ml[3,41+10])
        Tpat_constant Const_int 5
    ]
    None
  ",
                            "children": [
                              {
                                "filename": "test.ml",
                                "start": {
                                  "line": 3,
                                  "col": 9
                                },
                                "end": {
                                  "line": 3,
                                  "col": 10
                                },
                                "ghost": false,
                                "attrs": [],
                                "kind": "pattern (test.ml[3,41+9]..test.ml[3,41+10])
    Tpat_constant Const_int 5
  ",
                                "children": []
                              }
                            ]
                          },
                          {
                            "filename": "test.ml",
                            "start": {
                              "line": 3,
                              "col": 14
                            },
                            "end": {
                              "line": 3,
                              "col": 15
                            },
                            "ghost": false,
                            "attrs": [
                              {
                                "start": {
                                  "line": 3,
                                  "col": 13
                                },
                                "end": {
                                  "line": 3,
                                  "col": 15
                                },
                                "name": "merlin.loc"
                              }
                            ],
                            "kind": "expression",
                            "children": []
                          }
                        ]
                      },
                      {
                        "filename": "test.ml",
                        "start": {
                          "line": 4,
                          "col": 4
                        },
                        "end": {
                          "line": 4,
                          "col": 17
                        },
                        "ghost": false,
                        "attrs": [],
                        "kind": "case",
                        "children": [
                          {
                            "filename": "test.ml",
                            "start": {
                              "line": 4,
                              "col": 4
                            },
                            "end": {
                              "line": 4,
                              "col": 12
                            },
                            "ghost": false,
                            "attrs": [],
                            "kind": "pattern (test.ml[4,57+4]..test.ml[4,57+12])
    Tpat_construct \"Some\"
    [
      pattern (test.ml[4,57+9]..test.ml[4,57+12])
        Tpat_var \"_aa/279\"
    ]
    None
  ",
                            "children": [
                              {
                                "filename": "test.ml",
                                "start": {
                                  "line": 4,
                                  "col": 9
                                },
                                "end": {
                                  "line": 4,
                                  "col": 12
                                },
                                "ghost": false,
                                "attrs": [],
                                "kind": "pattern (test.ml[4,57+9]..test.ml[4,57+12])
    Tpat_var \"_aa/279\"
  ",
                                "children": []
                              }
                            ]
                          },
                          {
                            "filename": "test.ml",
                            "start": {
                              "line": 4,
                              "col": 16
                            },
                            "end": {
                              "line": 4,
                              "col": 17
                            },
                            "ghost": false,
                            "attrs": [
                              {
                                "start": {
                                  "line": 4,
                                  "col": 15
                                },
                                "end": {
                                  "line": 4,
                                  "col": 17
                                },
                                "name": "merlin.loc"
                              }
                            ],
                            "kind": "expression",
                            "children": []
                          }
                        ]
                      }
                    ]
                  }
                ]
              }
            ]
          }
        ]
      }
    ],
    "notifications": []
  }
