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
      ERROR_locate_from_inside_function_literal_used_as_non_function/277
        module_expr (test.ml[1,0+72]..test.ml[3,104+28])
          Tmod_structure
          [
            structure_item (test.ml[2,79+2]..test.ml[2,79+24])
              Tstr_value Nonrec
              [
                <def>
                  pattern (test.ml[2,79+6]..test.ml[2,79+13])
                    Tpat_var \"problem/275\"
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
