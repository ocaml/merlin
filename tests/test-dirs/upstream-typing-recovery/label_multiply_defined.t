  $ cat >label_multiply_defined.ml <<'EOF'
  > type t = { x : int; y : int }
  > 
  > let _ =
  >   { x = 1; y = 2; x = 3 }
  > 
  > let x = { x = 10; y = 11}
  > 
  > let _ =
  >   { x = 1; y = 2; y = 3 }
  > 
  > let t : string = 10
  > EOF

  $ $MERLIN single errors -filename label_multiply_defined.ml < label_multiply_defined.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 2
        },
        "end": {
          "line": 4,
          "col": 25
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The record field label x is defined several times"
      },
      {
        "start": {
          "line": 9,
          "col": 2
        },
        "end": {
          "line": 9,
          "col": 25
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The record field label y is defined several times"
      },
      {
        "start": {
          "line": 11,
          "col": 17
        },
        "end": {
          "line": 11,
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
