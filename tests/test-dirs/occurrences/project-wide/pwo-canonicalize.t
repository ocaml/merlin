  $ cat > lib.ml << EOF
  > let foo = "bar"
  > let () = print_string foo
  > EOF

  $ cat > main.ml << EOF
  > let () = print_string Lib.foo
  > EOF

  $ cat > .merlin << EOF
  > INDEX project.ocaml-index
  > SOURCE_ROOT .
  > EOF

  $ $OCAMLC -bin-annot -bin-annot-occurrences -c lib.ml main.ml
  $ ocaml-index aggregate main.cmt lib.cmt --root . --rewrite-root

  $ $MERLIN single occurrences -scope project -identifier-at 1:4 \
  > -filename lib.ml < lib.ml | jq .value
  [
    {
      "file": "$TESTCASE_ROOT/lib.ml",
      "start": {
        "line": 1,
        "col": 4
      },
      "end": {
        "line": 1,
        "col": 7
      },
      "stale": false
    },
    {
      "file": "$TESTCASE_ROOT/main.ml",
      "start": {
        "line": 1,
        "col": 26
      },
      "end": {
        "line": 1,
        "col": 29
      },
      "stale": false
    },
    {
      "file": "$TESTCASE_ROOT/lib.ml",
      "start": {
        "line": 2,
        "col": 22
      },
      "end": {
        "line": 2,
        "col": 25
      },
      "stale": false
    }
  ]
