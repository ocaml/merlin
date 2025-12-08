  $ cat >main.ml <<EOF
  > let a : type b. 'a as b list = []
  > EOF

FIXME: This syntactically incorrect OCaml snippet produces two unrelated cryptic error message.
See https://github.com/ocaml/merlin/issues/2000.
  $ $MERLIN single errors -filename main.ml <main.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 4
        },
        "end": {
          "line": 1,
          "col": 33
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This definition has type bool which is less general than 'a. 'a"
      },
      {
        "start": {
          "line": 1,
          "col": 21
        },
        "end": {
          "line": 1,
          "col": 23
        },
        "type": "parser",
        "sub": [],
        "valid": true,
        "message": "In this scoped type, variable 'b is reserved for the local type b."
      }
    ],
    "notifications": []
  }
