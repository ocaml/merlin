  $ cat >lib.ml <<'EOF'
  > (* blah *)
  > let foo = "bar"
  > EOF

  $ cat >main.ml <<'EOF'
  > let () = print_string Lib.foo
  > EOF

  $ $OCAMLC -bin-annot -bin-annot-occurrences -c lib.ml main.ml

  $ ocaml-index aggregate main.cmt lib.cmt

Foo was defined on line 2 when the index was built, but is now defined on line 1
  $ cat >lib.ml <<'EOF'
  > let foo = "bar"
  > EOF

  $ $MERLIN single occurrences -scope project -identifier-at 1:28 \
  > -index-file project.ocaml-index \
  > -filename main.ml < main.ml | jq .value
  [
    {
      "file": "$TESTCASE_ROOT/lib.ml",
      "start": {
        "line": 2,
        "col": 4
      },
      "end": {
        "line": 2,
        "col": 7
      },
      "stale": true
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
      }
    }
  ]
