  $ echo "S .\nB .\nFLG -nopervasives" > .merlin
  $ $OCAMLC -nopervasives -c -bin-annot lib.mli
  $ $MERLIN single errors -filename main.ml < main.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 8
        },
        "end": {
          "line": 2,
          "col": 12
        },
        "type": "warning",
        "sub": [],
        "valid": true,
        "message": "Alert deprecated: Lib.sqrt
  I am deprecated"
      }
    ],
    "notifications": []
  }

  $ cat > .merlin <<EOF
  > S .
  > B .
  > FLG -nopervasives -alert -deprecated
  > EOF

  $ $MERLIN single errors -filename main.ml < main.ml
  {
    "class": "return",
    "value": [],
    "notifications": []
  }

  $ cat > .merlin <<EOF
  > S .
  > B .
  > FLG -nopervasives -alert=-deprecated
  > EOF

  $ $MERLIN single errors -filename main.ml < main.ml
  {
    "class": "return",
    "value": [],
    "notifications": []
  }

The compiler accept both
  $ $OCAMLC -c main.ml
  File "main.ml", line 2, characters 8-12:
  2 | let x = sqrt 3.
              ^^^^
  Alert deprecated: Lib.sqrt
  I am deprecated

  $ $OCAMLC -alert -deprecated -c main.ml

  $ $OCAMLC -alert=-deprecated -c main.ml
