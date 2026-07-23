  $ cat >unknown_literal.ml <<'EOF'
  > let a = 123455z
  > let b = 990098_09988777k
  > 
  > let u = match (a, b) with
  >   | 123455z -> "foo"
  >   |990098_09988777k -> "bar"
  > 
  > let r = u ^ "baz"
  > let k : string = 10
  > EOF

  $ $MERLIN single errors -filename unknown_literal.ml < unknown_literal.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 8
        },
        "end": {
          "line": 1,
          "col": 15
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unknown modifier z for literal 123455z"
      },
      {
        "start": {
          "line": 2,
          "col": 8
        },
        "end": {
          "line": 2,
          "col": 24
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unknown modifier k for literal 990098_09988777k"
      },
      {
        "start": {
          "line": 5,
          "col": 4
        },
        "end": {
          "line": 5,
          "col": 11
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unknown modifier z for literal 123455z"
      },
      {
        "start": {
          "line": 6,
          "col": 3
        },
        "end": {
          "line": 6,
          "col": 19
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unknown modifier k for literal 990098_09988777k"
      },
      {
        "start": {
          "line": 9,
          "col": 17
        },
        "end": {
          "line": 9,
          "col": 19
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The constant 10 has type int but an expression was expected of type string"
      }
    ],
    "notifications": []
  }
