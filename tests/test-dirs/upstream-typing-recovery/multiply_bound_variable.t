  $ cat >multiply_bound_variable.ml <<'EOF'
  > let (x, x) = (1, 2)
  > 
  > let y = 10
  > 
  > let f = function
  >   | (x, x, y) -> x + y
  >   | _ -> 0
  > 
  > 
  > let z = f 10 + y
  > 
  > let a : string = 10
  > EOF

  $ $MERLIN single errors -filename multiply_bound_variable.ml < multiply_bound_variable.ml
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
          "col": 9
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Variable x is bound several times in this matching"
      },
      {
        "start": {
          "line": 6,
          "col": 8
        },
        "end": {
          "line": 6,
          "col": 9
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Variable x is bound several times in this matching"
      },
      {
        "start": {
          "line": 10,
          "col": 10
        },
        "end": {
          "line": 10,
          "col": 12
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The constant 10 has type int but an expression was expected of type
    'a * int * int"
      },
      {
        "start": {
          "line": 12,
          "col": 17
        },
        "end": {
          "line": 12,
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
