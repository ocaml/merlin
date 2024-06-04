  $ cat >lib.ml <<'EOF'
  > let foo = "bar"
  > let () = print_string foo
  > EOF

  $ cat >main.ml <<'EOF'
  > let () = print_string Lib.foo
  > EOF

  $ ocamlc -bin-annot -bin-annot-occurrences -c lib.ml main.ml

  $ ocaml-index aggregate main.cmt lib.cmt
  $ ocaml-index dump project.ocaml-index
  2 uids:
  {uid: Lib.0; locs:
     "foo": File "lib.ml", line 1, characters 4-7;
     "foo": File "lib.ml", line 2, characters 22-25;
     "Lib.foo": File "main.ml", line 1, characters 22-29
   uid: Stdlib.312; locs:
     "print_string": File "lib.ml", line 2, characters 9-21;
     "print_string": File "main.ml", line 1, characters 9-21
   }, 0 approx shapes: {}, and shapes for CUS .

  $ $MERLIN single occurrences -scope project -identifier-at 1:28 \
  > -index-file project.ocaml-index \
  > -filename main.ml <main.ml
  {
    "class": "return",
    "value": [
      {
        "file": "$TESTCASE_ROOT/lib.ml",
        "start": {
          "line": 1,
          "col": 4
        },
        "end": {
          "line": 1,
          "col": 7
        }
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
        }
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
    ],
    "notifications": []
  }
