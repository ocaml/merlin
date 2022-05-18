  $ dune build
  [
    structure_item (_none_[0,0+-1]..[0,0+-1]) ghost
      Pstr_attribute "ocaml.ppx.context"
      [
        structure_item (_none_[0,0+-1]..[0,0+-1]) ghost
          Pstr_eval
          expression (_none_[0,0+-1]..[0,0+-1]) ghost
            Pexp_record
            [
              "tool_name" (_none_[0,0+-1]..[0,0+-1]) ghost
                expression (_none_[0,0+-1]..[0,0+-1]) ghost
                  Pexp_constant PConst_string("ppx_driver",(_none_[0,0+-1]..[0,0+-1]) ghost,None)
              "include_dirs" (_none_[0,0+-1]..[0,0+-1]) ghost
                expression (_none_[0,0+-1]..[0,0+-1]) ghost
                  Pexp_construct "[]" (_none_[0,0+-1]..[0,0+-1]) ghost
                  None
              "load_path" (_none_[0,0+-1]..[0,0+-1]) ghost
                expression (_none_[0,0+-1]..[0,0+-1]) ghost
                  Pexp_construct "[]" (_none_[0,0+-1]..[0,0+-1]) ghost
                  None
              "open_modules" (_none_[0,0+-1]..[0,0+-1]) ghost
                expression (_none_[0,0+-1]..[0,0+-1]) ghost
                  Pexp_construct "[]" (_none_[0,0+-1]..[0,0+-1]) ghost
                  None
              "for_package" (_none_[0,0+-1]..[0,0+-1]) ghost
                expression (_none_[0,0+-1]..[0,0+-1]) ghost
                  Pexp_construct "None" (_none_[0,0+-1]..[0,0+-1]) ghost
                  None
              "debug" (_none_[0,0+-1]..[0,0+-1]) ghost
                expression (_none_[0,0+-1]..[0,0+-1]) ghost
                  Pexp_construct "false" (_none_[0,0+-1]..[0,0+-1]) ghost
                  None
              "use_threads" (_none_[0,0+-1]..[0,0+-1]) ghost
                expression (_none_[0,0+-1]..[0,0+-1]) ghost
                  Pexp_construct "false" (_none_[0,0+-1]..[0,0+-1]) ghost
                  None
              "use_vmthreads" (_none_[0,0+-1]..[0,0+-1]) ghost
                expression (_none_[0,0+-1]..[0,0+-1]) ghost
                  Pexp_construct "false" (_none_[0,0+-1]..[0,0+-1]) ghost
                  None
              "recursive_types" (_none_[0,0+-1]..[0,0+-1]) ghost
                expression (_none_[0,0+-1]..[0,0+-1]) ghost
                  Pexp_construct "false" (_none_[0,0+-1]..[0,0+-1]) ghost
                  None
              "principal" (_none_[0,0+-1]..[0,0+-1]) ghost
                expression (_none_[0,0+-1]..[0,0+-1]) ghost
                  Pexp_construct "false" (_none_[0,0+-1]..[0,0+-1]) ghost
                  None
              "transparent_modules" (_none_[0,0+-1]..[0,0+-1]) ghost
                expression (_none_[0,0+-1]..[0,0+-1]) ghost
                  Pexp_construct "false" (_none_[0,0+-1]..[0,0+-1]) ghost
                  None
              "unboxed_types" (_none_[0,0+-1]..[0,0+-1]) ghost
                expression (_none_[0,0+-1]..[0,0+-1]) ghost
                  Pexp_construct "false" (_none_[0,0+-1]..[0,0+-1]) ghost
                  None
              "unsafe_string" (_none_[0,0+-1]..[0,0+-1]) ghost
                expression (_none_[0,0+-1]..[0,0+-1]) ghost
                  Pexp_construct "false" (_none_[0,0+-1]..[0,0+-1]) ghost
                  None
              "cookies" (_none_[0,0+-1]..[0,0+-1]) ghost
                expression (_none_[0,0+-1]..[0,0+-1]) ghost
                  Pexp_construct "::" (_none_[0,0+-1]..[0,0+-1]) ghost
                  Some
                    expression (_none_[0,0+-1]..[0,0+-1]) ghost
                      Pexp_tuple
                      [
                        expression (_none_[0,0+-1]..[0,0+-1]) ghost
                          Pexp_tuple
                          [
                            expression (_none_[0,0+-1]..[0,0+-1]) ghost
                              Pexp_constant PConst_string("library-name",(_none_[0,0+-1]..[0,0+-1]) ghost,None)
                            expression (<command-line>[1,0+0]..[1,0+6])
                              Pexp_constant PConst_string("lib1",(<command-line>[1,0+1]..[1,0+5]),None)
                          ]
                        expression (_none_[0,0+-1]..[0,0+-1]) ghost
                          Pexp_construct "[]" (_none_[0,0+-1]..[0,0+-1]) ghost
                          None
                      ]
            ]
            None
      ]
    structure_item (lib1.ml[1,0+0]..[1,0+29])
      Pstr_value Nonrec
      [
        <def>
          pattern (lib1.ml[1,0+4]..[1,0+7])
            Ppat_var "boo" (lib1.ml[1,0+4]..[1,0+7])
          expression (lib1.ml[1,0+8]..[1,0+29]) ghost
            Pexp_fun
            Nolabel
            None
            pattern (lib1.ml[1,0+8]..[1,0+9])
              Ppat_var "r" (lib1.ml[1,0+8]..[1,0+9])
            expression (lib1.ml[1,0+12]..[1,0+17])
              Pexp_apply
              expression (lib1.ml[1,0+12]..[1,0+17])
                Pexp_ident "call_fresh" (lib1.ml[1,0+12]..[1,0+17])
              [
                <arg>
                Nolabel
                  expression (lib1.ml[1,0+12]..[1,0+17])
                    Pexp_fun
                    Nolabel
                    None
                    pattern (lib1.ml[1,0+18]..[1,0+19])
                      Ppat_var "a" (lib1.ml[1,0+12]..[1,0+29])
                    expression (lib1.ml[1,0+20]..[1,0+29])
                      Pexp_apply
                      expression (lib1.ml[1,0+23]..[1,0+26])
                        Pexp_ident "===" (lib1.ml[1,0+23]..[1,0+26])
                      [
                        <arg>
                        Nolabel
                          expression (lib1.ml[1,0+21]..[1,0+22])
                            Pexp_ident "r" (lib1.ml[1,0+21]..[1,0+22])
                        <arg>
                        Nolabel
                          expression (lib1.ml[1,0+27]..[1,0+28])
                            Pexp_ident "a" (lib1.ml[1,0+27]..[1,0+28])
                      ]
              ]
      ]
    structure_item (lib1.ml[2,30+0]..[2,30+50])
      Pstr_value Nonrec
      [
        <def>
          pattern (lib1.ml[2,30+4]..[2,30+16])
            Ppat_var "boo_expanded" (lib1.ml[2,30+4]..[2,30+16])
          expression (lib1.ml[2,30+17]..[2,30+50]) ghost
            Pexp_fun
            Nolabel
            None
            pattern (lib1.ml[2,30+17]..[2,30+18])
              Ppat_var "r" (lib1.ml[2,30+17]..[2,30+18])
            expression (lib1.ml[2,30+21]..[2,30+50])
              Pexp_apply
              expression (lib1.ml[2,30+21]..[2,30+31])
                Pexp_ident "call_fresh" (lib1.ml[2,30+21]..[2,30+31])
              [
                <arg>
                Nolabel
                  expression (lib1.ml[2,30+32]..[2,30+50])
                    Pexp_fun
                    Nolabel
                    None
                    pattern (lib1.ml[2,30+37]..[2,30+38])
                      Ppat_var "a" (lib1.ml[2,30+37]..[2,30+38])
                    expression (lib1.ml[2,30+42]..[2,30+49])
                      Pexp_apply
                      expression (lib1.ml[2,30+44]..[2,30+47])
                        Pexp_ident "===" (lib1.ml[2,30+44]..[2,30+47])
                      [
                        <arg>
                        Nolabel
                          expression (lib1.ml[2,30+42]..[2,30+43])
                            Pexp_ident "r" (lib1.ml[2,30+42]..[2,30+43])
                        <arg>
                        Nolabel
                          expression (lib1.ml[2,30+48]..[2,30+49])
                            Pexp_ident "a" (lib1.ml[2,30+48]..[2,30+49])
                      ]
              ]
      ]
  ]
  
  [
    structure_item (_none_[0,0+-1]..[0,0+-1]) ghost
      Pstr_attribute "ocaml.ppx.context"
      [
        structure_item (_none_[0,0+-1]..[0,0+-1]) ghost
          Pstr_eval
          expression (_none_[0,0+-1]..[0,0+-1]) ghost
            Pexp_record
            [
              "tool_name" (_none_[0,0+-1]..[0,0+-1]) ghost
                expression (_none_[0,0+-1]..[0,0+-1]) ghost
                  Pexp_constant PConst_string("ppx_driver",(_none_[0,0+-1]..[0,0+-1]) ghost,None)
              "include_dirs" (_none_[0,0+-1]..[0,0+-1]) ghost
                expression (_none_[0,0+-1]..[0,0+-1]) ghost
                  Pexp_construct "[]" (_none_[0,0+-1]..[0,0+-1]) ghost
                  None
              "load_path" (_none_[0,0+-1]..[0,0+-1]) ghost
                expression (_none_[0,0+-1]..[0,0+-1]) ghost
                  Pexp_construct "[]" (_none_[0,0+-1]..[0,0+-1]) ghost
                  None
              "open_modules" (_none_[0,0+-1]..[0,0+-1]) ghost
                expression (_none_[0,0+-1]..[0,0+-1]) ghost
                  Pexp_construct "[]" (_none_[0,0+-1]..[0,0+-1]) ghost
                  None
              "for_package" (_none_[0,0+-1]..[0,0+-1]) ghost
                expression (_none_[0,0+-1]..[0,0+-1]) ghost
                  Pexp_construct "None" (_none_[0,0+-1]..[0,0+-1]) ghost
                  None
              "debug" (_none_[0,0+-1]..[0,0+-1]) ghost
                expression (_none_[0,0+-1]..[0,0+-1]) ghost
                  Pexp_construct "false" (_none_[0,0+-1]..[0,0+-1]) ghost
                  None
              "use_threads" (_none_[0,0+-1]..[0,0+-1]) ghost
                expression (_none_[0,0+-1]..[0,0+-1]) ghost
                  Pexp_construct "false" (_none_[0,0+-1]..[0,0+-1]) ghost
                  None
              "use_vmthreads" (_none_[0,0+-1]..[0,0+-1]) ghost
                expression (_none_[0,0+-1]..[0,0+-1]) ghost
                  Pexp_construct "false" (_none_[0,0+-1]..[0,0+-1]) ghost
                  None
              "recursive_types" (_none_[0,0+-1]..[0,0+-1]) ghost
                expression (_none_[0,0+-1]..[0,0+-1]) ghost
                  Pexp_construct "false" (_none_[0,0+-1]..[0,0+-1]) ghost
                  None
              "principal" (_none_[0,0+-1]..[0,0+-1]) ghost
                expression (_none_[0,0+-1]..[0,0+-1]) ghost
                  Pexp_construct "false" (_none_[0,0+-1]..[0,0+-1]) ghost
                  None
              "transparent_modules" (_none_[0,0+-1]..[0,0+-1]) ghost
                expression (_none_[0,0+-1]..[0,0+-1]) ghost
                  Pexp_construct "false" (_none_[0,0+-1]..[0,0+-1]) ghost
                  None
              "unboxed_types" (_none_[0,0+-1]..[0,0+-1]) ghost
                expression (_none_[0,0+-1]..[0,0+-1]) ghost
                  Pexp_construct "false" (_none_[0,0+-1]..[0,0+-1]) ghost
                  None
              "unsafe_string" (_none_[0,0+-1]..[0,0+-1]) ghost
                expression (_none_[0,0+-1]..[0,0+-1]) ghost
                  Pexp_construct "false" (_none_[0,0+-1]..[0,0+-1]) ghost
                  None
              "cookies" (_none_[0,0+-1]..[0,0+-1]) ghost
                expression (_none_[0,0+-1]..[0,0+-1]) ghost
                  Pexp_construct "::" (_none_[0,0+-1]..[0,0+-1]) ghost
                  Some
                    expression (_none_[0,0+-1]..[0,0+-1]) ghost
                      Pexp_tuple
                      [
                        expression (_none_[0,0+-1]..[0,0+-1]) ghost
                          Pexp_tuple
                          [
                            expression (_none_[0,0+-1]..[0,0+-1]) ghost
                              Pexp_constant PConst_string("library-name",(_none_[0,0+-1]..[0,0+-1]) ghost,None)
                            expression (<command-line>[1,0+0]..[1,0+6])
                              Pexp_constant PConst_string("lib1",(<command-line>[1,0+1]..[1,0+5]),None)
                          ]
                        expression (_none_[0,0+-1]..[0,0+-1]) ghost
                          Pexp_construct "[]" (_none_[0,0+-1]..[0,0+-1]) ghost
                          None
                      ]
            ]
            None
      ]
    structure_item (lib1.ml[1,0+0]..[1,0+29])
      Pstr_value Nonrec
      [
        <def>
          pattern (lib1.ml[1,0+4]..[1,0+7])
            Ppat_var "boo" (lib1.ml[1,0+4]..[1,0+7])
          expression (lib1.ml[1,0+8]..[1,0+29]) ghost
            Pexp_fun
            Nolabel
            None
            pattern (lib1.ml[1,0+8]..[1,0+9])
              Ppat_var "r" (lib1.ml[1,0+8]..[1,0+9])
            expression (lib1.ml[1,0+12]..[1,0+17])
              Pexp_apply
              expression (lib1.ml[1,0+12]..[1,0+17])
                Pexp_ident "call_fresh" (lib1.ml[1,0+12]..[1,0+17])
              [
                <arg>
                Nolabel
                  expression (lib1.ml[1,0+12]..[1,0+17])
                    Pexp_fun
                    Nolabel
                    None
                    pattern (lib1.ml[1,0+18]..[1,0+19])
                      Ppat_var "a" (lib1.ml[1,0+12]..[1,0+29])
                    expression (lib1.ml[1,0+20]..[1,0+29])
                      Pexp_apply
                      expression (lib1.ml[1,0+23]..[1,0+26])
                        Pexp_ident "===" (lib1.ml[1,0+23]..[1,0+26])
                      [
                        <arg>
                        Nolabel
                          expression (lib1.ml[1,0+21]..[1,0+22])
                            Pexp_ident "r" (lib1.ml[1,0+21]..[1,0+22])
                        <arg>
                        Nolabel
                          expression (lib1.ml[1,0+27]..[1,0+28])
                            Pexp_ident "a" (lib1.ml[1,0+27]..[1,0+28])
                      ]
              ]
      ]
    structure_item (lib1.ml[2,30+0]..[2,30+50])
      Pstr_value Nonrec
      [
        <def>
          pattern (lib1.ml[2,30+4]..[2,30+16])
            Ppat_var "boo_expanded" (lib1.ml[2,30+4]..[2,30+16])
          expression (lib1.ml[2,30+17]..[2,30+50]) ghost
            Pexp_fun
            Nolabel
            None
            pattern (lib1.ml[2,30+17]..[2,30+18])
              Ppat_var "r" (lib1.ml[2,30+17]..[2,30+18])
            expression (lib1.ml[2,30+21]..[2,30+50])
              Pexp_apply
              expression (lib1.ml[2,30+21]..[2,30+31])
                Pexp_ident "call_fresh" (lib1.ml[2,30+21]..[2,30+31])
              [
                <arg>
                Nolabel
                  expression (lib1.ml[2,30+32]..[2,30+50])
                    Pexp_fun
                    Nolabel
                    None
                    pattern (lib1.ml[2,30+37]..[2,30+38])
                      Ppat_var "a" (lib1.ml[2,30+37]..[2,30+38])
                    expression (lib1.ml[2,30+42]..[2,30+49])
                      Pexp_apply
                      expression (lib1.ml[2,30+44]..[2,30+47])
                        Pexp_ident "===" (lib1.ml[2,30+44]..[2,30+47])
                      [
                        <arg>
                        Nolabel
                          expression (lib1.ml[2,30+42]..[2,30+43])
                            Pexp_ident "r" (lib1.ml[2,30+42]..[2,30+43])
                        <arg>
                        Nolabel
                          expression (lib1.ml[2,30+48]..[2,30+49])
                            Pexp_ident "a" (lib1.ml[2,30+48]..[2,30+49])
                      ]
              ]
      ]
  ]
  
  $ dune runtest
  $ cat lib1.ml | nl -ba
       1	let boo r = fresh a (r === a)
       2	let boo_expanded r = call_fresh (fun a -> r === a)
2:49 is a location of last `a` in `let boo_expanded r = call_fresh (fun a -> r === a)`
Reported occurences are correct
  $ ocamlmerlin single occurrences -identifier-at 2:49 -filename ./lib1.ml < ./lib1.ml
  {"class":"return","value":[{"start":{"line":2,"col":37},"end":{"line":2,"col":38}},{"start":{"line":2,"col":48},"end":{"line":2,"col":49}}],"notifications":[],"timing":{"clock":16,"cpu":12,"query":0,"pp":0,"reader":0,"ppx":10,"typer":1,"error":0}}
1.28 is a location of the last `a` in the piece of code being PPX-rewritten
occurences are empty, which is not expected.
Highlight in VsCode + lsp-server is extremly weird, see https://github.com/ocaml/ocaml-lsp/issues/147
  $ ocamlmerlin single occurrences -identifier-at 1:28 -filename ./lib1.ml < ./lib1.ml
  {"class":"return","value":[],"notifications":[],"timing":{"clock":19,"cpu":14,"query":0,"pp":0,"reader":0,"ppx":11,"typer":2,"error":0}}
