  $ cat >orpat_vars.ml <<'EOF'
  > let f = function
  >   | Some x | None -> x
  > 
  > let a = 10
  > 
  > let g = function
  >   | (x, y) | _ -> x
  > 
  > let b = a + 10
  > 
  > let h = function
  >   | (Some x, y) | (None, y) -> x
  > 
  > let c : string = 10
  > EOF

  $ $MERLIN single errors -filename orpat_vars.ml < orpat_vars.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 4
        },
        "end": {
          "line": 2,
          "col": 17
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Variable x must occur on both sides of this | pattern"
      },
      {
        "start": {
          "line": 2,
          "col": 21
        },
        "end": {
          "line": 2,
          "col": 22
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound value x"
      },
      {
        "start": {
          "line": 7,
          "col": 4
        },
        "end": {
          "line": 7,
          "col": 14
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Variable x must occur on both sides of this | pattern"
      },
      {
        "start": {
          "line": 7,
          "col": 18
        },
        "end": {
          "line": 7,
          "col": 19
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound value x"
      },
      {
        "start": {
          "line": 12,
          "col": 4
        },
        "end": {
          "line": 12,
          "col": 27
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Variable x must occur on both sides of this | pattern"
      },
      {
        "start": {
          "line": 12,
          "col": 31
        },
        "end": {
          "line": 12,
          "col": 32
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound value x"
      },
      {
        "start": {
          "line": 14,
          "col": 17
        },
        "end": {
          "line": 14,
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
