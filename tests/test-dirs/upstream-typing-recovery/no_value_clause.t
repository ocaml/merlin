  $ cat >no_value_clause.ml <<'EOF'
  > let f x = match x with
  >   | exception Not_found -> 12
  > 
  > let x = (f ()) + 12
  > 
  > let g x =
  >   let a = match x with
  >     | exception Not_found -> 1
  >     | exception Failure _ -> 2
  >   in a + 10
  > 
  > let t : string = x + (g 10)
  > EOF

  $ $MERLIN single errors -filename no_value_clause.ml < no_value_clause.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 10
        },
        "end": {
          "line": 2,
          "col": 29
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "None of the patterns in this match expression match values."
      },
      {
        "start": {
          "line": 7,
          "col": 10
        },
        "end": {
          "line": 9,
          "col": 30
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "None of the patterns in this match expression match values."
      },
      {
        "start": {
          "line": 12,
          "col": 17
        },
        "end": {
          "line": 12,
          "col": 27
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type int but an expression was expected of type string"
      }
    ],
    "notifications": []
  }
