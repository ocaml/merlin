  $ cat >unify_errors.ml <<'EOF'
  > let x : int = "hello"
  > 
  > let f b =
  >   match b with
  >   | true -> 123
  >   | false -> "not a number"
  > 
  > let rec f x =
  >   1 + f [x]
  > 
  > let y = x + 10
  > EOF

  $ $MERLIN single errors -filename unify_errors.ml < unify_errors.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 14
        },
        "end": {
          "line": 1,
          "col": 21
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This constant has type string but an expression was expected of type int"
      },
      {
        "start": {
          "line": 6,
          "col": 13
        },
        "end": {
          "line": 6,
          "col": 27
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This constant has type string but an expression was expected of type int"
      },
      {
        "start": {
          "line": 9,
          "col": 9
        },
        "end": {
          "line": 9,
          "col": 10
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The value x has type 'a list but an expression was expected of type 'a
  The type variable 'b occurs inside 'b list"
      }
    ],
    "notifications": []
  }
