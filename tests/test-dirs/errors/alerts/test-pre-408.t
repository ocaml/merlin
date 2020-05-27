(enabled_if (< %{ocaml_version} 4.08.0))

  $ dune build @check
  File "main.ml", line 2, characters 8-12:
  Warning 3: deprecated: Lib.sqrt
  I am deprecated
  File "main.ml", line 1:
  Error: Some fatal warnings were triggered (1 occurrences)
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
        "type": "warning",
        "sub": [],
        "valid": true,
        "message": "Warning 3: deprecated: Lib.sqrt
  I am deprecated"
      }
    ],
    "notifications": []
  }
