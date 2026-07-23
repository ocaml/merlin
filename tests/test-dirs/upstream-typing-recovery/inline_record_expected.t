  $ cat >inline_record_expected.ml <<'EOF'
  > type t = A of { x: int }
  > 
  > let x = A 1
  > let y =
  >   let a = A 2 in
  >   match (a, x) with
  >   | A {x}, A {x = y} -> x + y
  > 
  > let u = y + y + 2
  > let t : string = u
  > EOF

  $ $MERLIN single errors -filename inline_record_expected.ml < inline_record_expected.ml
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
        "message": "This constructor expects an inlined record argument."
      },
      {
        "start": {
          "line": 5,
          "col": 10
        },
        "end": {
          "line": 5,
          "col": 13
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This constructor expects an inlined record argument."
      },
      {
        "start": {
          "line": 10,
          "col": 17
        },
        "end": {
          "line": 10,
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
