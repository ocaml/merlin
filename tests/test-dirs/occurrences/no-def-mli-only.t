  $ cat >noml.mli <<'EOF'
  > type t = unit
  > EOF

  $ cat >noml.ml <<'EOF'
  > type t = unit
  > EOF

  $ cat >main.ml <<'EOF'
  > let x : Noml.t = ()
  > let y : Noml.t = ()
  > EOF

  $ $OCAMLC -c -bin-annot noml.mli noml.ml main.ml

We remove the source file to mimick cases were generated source files are not
accessible to Merlin.
  $ rm noml.ml

We still expect occurrences of definitions in hidden source files to work
  $ $MERLIN single occurrences -identifier-at 2:13 -filename main.ml <main.ml  
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 13
        },
        "end": {
          "line": 1,
          "col": 14
        }
      },
      {
        "start": {
          "line": 2,
          "col": 13
        },
        "end": {
          "line": 2,
          "col": 14
        }
      }
    ],
    "notifications": []
  }
