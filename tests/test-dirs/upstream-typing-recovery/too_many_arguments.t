  $ cat >too_many_arguments.ml <<'EOF'
  > let f _ = 43
  > let y = f 10 11
  > let f: _ -> int = fun x y -> 0
  > 
  > let u = f 10
  > let t : string = 10
  > EOF

  $ $MERLIN single errors -filename too_many_arguments.ml < too_many_arguments.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 8
        },
        "end": {
          "line": 2,
          "col": 15
        },
        "type": "typer",
        "sub": [
          {
            "start": {
              "line": 2,
              "col": 8
            },
            "end": {
              "line": 2,
              "col": 15
            },
            "message": "This extra argument is not expected."
          }
        ],
        "valid": true,
        "message": "The function f has type 'b -> int
  It is applied to too many arguments"
      },
      {
        "start": {
          "line": 3,
          "col": 18
        },
        "end": {
          "line": 3,
          "col": 30
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This function expects too many arguments, it should have type 'a -> int"
      },
      {
        "start": {
          "line": 3,
          "col": 24
        },
        "end": {
          "line": 3,
          "col": 30
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type 'a -> int but an expression was expected of type int"
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
