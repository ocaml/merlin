  $ cat >or_pattern_type_clash.ml <<'EOF'
  > let _ =
  >   match (1, "hello") with
  >   | (x, _) | (_, x) -> x
  > 
  > let x = 10
  > 
  > let y = x ^ "ocaml"
  > EOF

  $ $MERLIN single errors -filename or_pattern_type_clash.ml < or_pattern_type_clash.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 4
        },
        "end": {
          "line": 3,
          "col": 19
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The variable x on the left-hand side of this or-pattern has type int
  but on the right-hand side it has type string"
      },
      {
        "start": {
          "line": 3,
          "col": 23
        },
        "end": {
          "line": 3,
          "col": 24
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound value x"
      },
      {
        "start": {
          "line": 7,
          "col": 8
        },
        "end": {
          "line": 7,
          "col": 9
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The value x has type int but an expression was expected of type string"
      }
    ],
    "notifications": []
  }
