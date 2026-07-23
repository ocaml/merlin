  $ cat >private_ctor.ml <<'EOF'
  > type t = ..
  > type t += A
  > type t += private B
  > 
  > 
  > let x = (A, B)
  > 
  > let y = (A, A)
  > 
  > let z =
  >   let a = B in
  >   let b = A in
  >   (a, b)
  > 
  > let t : int = ""
  > EOF

  $ $MERLIN single errors -filename private_ctor.ml < private_ctor.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 6,
          "col": 12
        },
        "end": {
          "line": 6,
          "col": 13
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Cannot use private constructor B to create values of type t"
      },
      {
        "start": {
          "line": 11,
          "col": 10
        },
        "end": {
          "line": 11,
          "col": 11
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Cannot use private constructor B to create values of type t"
      },
      {
        "start": {
          "line": 15,
          "col": 14
        },
        "end": {
          "line": 15,
          "col": 16
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This constant has type string but an expression was expected of type int"
      }
    ],
    "notifications": []
  }
