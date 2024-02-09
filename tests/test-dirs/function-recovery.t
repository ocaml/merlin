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
      ERROR_locate_from_inside_function_literal_used_as_non_function/278
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
                    Nolabel
                    [
                      <case>
                        pattern (test.ml[3,104+15]..test.ml[3,104+17])
                          Tpat_construct \"()\"
                          []
                          None
                        expression (test.ml[3,104+21]..test.ml[3,104+28])
                          attribute \"merlin.loc\"
                            []
                          Texp_ident \"problem/275\"
                    ]
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
            Tpat_var \"f/275\"
          expression (type.ml[1,0+8]..type.ml[1,0+61])
            extra
              Texp_newtype' \"t/276\"
            Texp_function
            Nolabel
            [
              <case>
                pattern (type.ml[1,0+22]..type.ml[1,0+25])
                  extra
                    Tpat_extra_constraint
                    core_type (type.ml[1,0+28]..type.ml[1,0+34])
                      Ttyp_constr \"list/9!\"
                      [
                        core_type (type.ml[1,0+28]..type.ml[1,0+29])
                          Ttyp_constr \"t/276\"
                          []
                      ]
                  Tpat_alias \"foo/277\"
                  pattern (type.ml[1,0+22]..type.ml[1,0+25])
                    Tpat_any
                expression (type.ml[1,0+39]..type.ml[1,0+61])
                  attribute \"merlin.loc\"
                    []
                  Texp_let Nonrec
                  [
                    <def>
                      pattern (type.ml[1,0+44]..type.ml[1,0+45])
                        extra
                          Tpat_extra_constraint
                          core_type (type.ml[1,0+48]..type.ml[1,0+49])
                            Ttyp_constr \"t/276\"
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
                        Texp_ident \"*type-error*/278\"
                  ]
                  expression (type.ml[1,0+59]..type.ml[1,0+61])
                    attribute \"merlin.loc\"
                      []
                    Texp_construct \"()\"
                    []
            ]
      ]
  ]
  
  
  ",
    "notifications": []
  }

