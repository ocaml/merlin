  $ cat >dune-project <<EOF
  > (lang dune 2.0)
  > EOF

  $ cat >main.ml <<EOF
  > let f x =
  >   [%string
  >     "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa \
  >      %{x}"]
  > ;;
  > print_endline @@ f "42";;
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (preprocess (pps ppx_string)))
  > EOF

  $ dune build @check 

  $ dune exec ./main.exe
  aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa      42

FIX upstream: locs issued by the ppx does not enable Merlin to work as expected
  $ $MERLIN single type-enclosing -position 3:7 \
  > -filename main.ml <main.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 6
        },
        "end": {
          "line": 3,
          "col": 97
        },
        "type": "string -> string",
        "tail": "no"
      }
    ],
    "notifications": []
  }

FIXME: Merlin should ignore hidden nodes in occurrences results to prevent
broken renaming
  $ $MERLIN single occurrences -identifier-at 1:6 \
  > -filename main.ml <main.ml

  $ $MERLIN single dump -what typedtree -filename main.ml <main.ml
  {
    "class": "return",
    "value": "[
    structure_item (main.ml[1,0+0]..main.ml[3,21+97])
      Tstr_value Nonrec
      [
        <def>
          pattern (main.ml[1,0+4]..main.ml[1,0+5])
            Tpat_var \"f/273\"
          expression (main.ml[1,0+6]..main.ml[3,21+97]) ghost
            Texp_function
            Nolabel
            [
              <case>
                pattern (main.ml[1,0+6]..main.ml[1,0+7])
                  Tpat_var \"x/275\"
                expression (main.ml[2,10+2]..main.ml[3,21+97])
                  attribute \"merlin.hide\"
                    []
                  Texp_apply
                  expression (main.ml[2,10+2]..main.ml[3,21+97])
                    Texp_ident \"Stdlib!.String.concat\"
                  [
                    <arg>
                      Nolabel
                      expression (main.ml[2,10+2]..main.ml[3,21+97])
                        Texp_constant Const_string(\"\",(main.ml[2,10+2]..main.ml[3,21+97]),None)
                    <arg>
                      Nolabel
                      expression (main.ml[2,10+2]..main.ml[3,21+97])
                        Texp_construct \"::\"
                        [
                          expression (main.ml[3,21+5]..main.ml[3,21+91]) ghost
                            Texp_constant Const_string(\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa      \",(main.ml[3,21+5]..main.ml[3,21+91]) ghost,None)
                          expression (main.ml[2,10+2]..main.ml[3,21+97])
                            Texp_construct \"::\"
                            [
                              expression (main.ml[3,21+93]..main.ml[3,21+94])
                                Texp_ident \"x/275\"
                              expression (main.ml[2,10+2]..main.ml[3,21+97])
                                Texp_construct \"[]\"
                                []
                            ]
                        ]
                  ]
            ]
      ]
    structure_item (main.ml[5,122+0]..main.ml[5,122+23])
      Tstr_eval
      expression (main.ml[5,122+0]..main.ml[5,122+23])
        Texp_apply
        expression (main.ml[5,122+0]..main.ml[5,122+13])
          Texp_ident \"Stdlib!.print_endline\"
        [
          <arg>
            Nolabel
            expression (main.ml[5,122+17]..main.ml[5,122+23])
              Texp_apply
              expression (main.ml[5,122+17]..main.ml[5,122+18])
                Texp_ident \"f/273\"
              [
                <arg>
                  Nolabel
                  expression (main.ml[5,122+19]..main.ml[5,122+23])
                    Texp_constant Const_string(\"42\",(main.ml[5,122+20]..main.ml[5,122+22]),None)
              ]
        ]
  ]
  
  
  ",
    "notifications": []
  }
