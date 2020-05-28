(enabled_if (>= %{ocaml_version} 4.08.0))

  $ dune build @check
  File "main.ml", line 2, characters 8-12:
  2 | let x = sqrt 3.
              ^^^^
  Error (alert deprecated): Lib.sqrt
  I am deprecated
  [1]

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
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Error (alert deprecated): Lib.sqrt
  I am deprecated"
      }
    ],
    "notifications": []
  }
