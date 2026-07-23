  $ cat >expr_not_a_record_type.ml <<'EOF'
  > type foo = { mutable y:int }
  > let f (r: int) = r.y <- 3
  > 
  > let _ = { true with contents = 0 }
  > 
  > let t : string = 10
  > EOF

  $ $MERLIN single errors -filename expr_not_a_record_type.ml < expr_not_a_record_type.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 17
        },
        "end": {
          "line": 2,
          "col": 18
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type int which is not a record type."
      },
      {
        "start": {
          "line": 4,
          "col": 10
        },
        "end": {
          "line": 4,
          "col": 14
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type bool which is not a record type."
      },
      {
        "start": {
          "line": 6,
          "col": 17
        },
        "end": {
          "line": 6,
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
