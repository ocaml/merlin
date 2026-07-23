  $ cat >label_missing.ml <<'EOF'
  > type t = {x: int; y: int; z: int}
  > 
  > let _ = { x = 10}
  > 
  > (* Here, the label z is also missing, but since the error share the same
  >    location of the multiple label, the next error is ommited. *)
  > let _ = {x = 10; y = 10; y = 11}
  > 
  > let _ = { x = 10; y = 10; z = 10}
  > 
  > let x : string = 10
  > EOF

  $ $MERLIN single errors -filename label_missing.ml < label_missing.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 8
        },
        "end": {
          "line": 3,
          "col": 17
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Some record fields are undefined: y z"
      },
      {
        "start": {
          "line": 7,
          "col": 8
        },
        "end": {
          "line": 7,
          "col": 32
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
