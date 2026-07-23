  $ cat >inside_mli.ml <<'EOF'
  > let f _ = Ok 10.0
  > let g _ = Some 10
  > EOF

  $ cat >inside_mli.mli <<'EOF'
  > val f : int -> float result
  > val g : unit -> (int, float) option
  > EOF

  $ $MERLIN single errors -filename inside_mli.mli < inside_mli.mli
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 15
        },
        "end": {
          "line": 1,
          "col": 27
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The type constructor result expects 2 argument(s),
  but is here applied to 1 argument(s)"
      },
      {
        "start": {
          "line": 2,
          "col": 16
        },
        "end": {
          "line": 2,
          "col": 35
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The type constructor option expects 1 argument(s),
  but is here applied to 2 argument(s)"
      }
    ],
    "notifications": []
  }
