  $ cat >inlined_record_escape.ml <<'EOF'
  > type t = A of {x:int}
  > let f = function (A (x:_)) -> 0
  > 
  > let g x =
  >   let a = match x with
  >     | A y -> y
  >   in
  >   a.x + 10
  > 
  > let u = f (A {x = 11}) + g (A {x = 10})
  > let t : string = u
  > EOF

  $ $MERLIN single errors -filename inlined_record_escape.ml < inlined_record_escape.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 20
        },
        "end": {
          "line": 2,
          "col": 25
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This form is not allowed as the type of the inlined record could escape."
      },
      {
        "start": {
          "line": 6,
          "col": 13
        },
        "end": {
          "line": 6,
          "col": 14
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This form is not allowed as the type of the inlined record could escape."
      },
      {
        "start": {
          "line": 8,
          "col": 4
        },
        "end": {
          "line": 8,
          "col": 5
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound record field x"
      },
      {
        "start": {
          "line": 11,
          "col": 17
        },
        "end": {
          "line": 11,
          "col": 18
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The value u has type int but an expression was expected of type string"
      }
    ],
    "notifications": []
  }
