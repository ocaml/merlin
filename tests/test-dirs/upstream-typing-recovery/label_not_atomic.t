  $ cat >label_not_atomic.ml <<'EOF'
  > module E = struct
  >   type t = { x : int; mutable y : string [@atomic] }
  >   let f t = [%atomic.loc t.x], [%atomic.loc t.y]
  > end
  > 
  > let f () =
  >   let (a, b) = E.f E.{x = 10; y = "11"} in
  >   a + (int_of_string b)
  > 
  > let t : string = f ()
  > EOF

  $ $MERLIN single errors -filename label_not_atomic.ml < label_not_atomic.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 12
        },
        "end": {
          "line": 3,
          "col": 29
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The record field x is not atomic"
      },
      {
        "start": {
          "line": 8,
          "col": 21
        },
        "end": {
          "line": 8,
          "col": 22
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The value b has type string atomic_loc but an expression was expected of type
    string"
      },
      {
        "start": {
          "line": 10,
          "col": 17
        },
        "end": {
          "line": 10,
          "col": 21
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type int but an expression was expected of type string"
      }
    ],
    "notifications": []
  }
