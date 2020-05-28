(enabled_if (and (>= %{ocaml_version} 4.06.0) (< %{ocaml_version} 4.08.0)))

  $ dune build @check
  File "main.ml", line 2, characters 8-12:
  Error (warning 3): deprecated: Lib.sqrt
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
        "message": "Error (warning 3): deprecated: Lib.sqrt
  I am deprecated"
      }
    ],
    "notifications": []
  }
