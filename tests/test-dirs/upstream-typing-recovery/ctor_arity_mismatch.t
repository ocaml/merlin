  $ cat >ctor_arity_mismatch.ml <<'EOF'
  > type t = A of int * string
  > 
  > let x = A 1
  > 
  > let f = match x with
  >   | A (y, _) -> y + 10
  > 
  > let y = x + "foo"
  > EOF

  $ $MERLIN single errors -filename ctor_arity_mismatch.ml < ctor_arity_mismatch.ml
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
          "col": 11
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The constructor A expects 2 argument(s), but is applied here to 1 argument(s)"
      },
      {
        "start": {
          "line": 8,
          "col": 12
        },
        "end": {
          "line": 8,
          "col": 17
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This constant has type string but an expression was expected of type int"
      }
    ],
    "notifications": []
  }
