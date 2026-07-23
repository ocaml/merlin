  $ cat >mutual_recursion.ml <<'EOF'
  > let rec foo x = x + a + b
  > and (a,b) = (1,2)
  > 
  > let c = foo 10 (* Should Work *)
  > 
  > let d = 10 + "foo"
  > EOF

  $ $MERLIN single errors -filename mutual_recursion.ml < mutual_recursion.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 4
        },
        "end": {
          "line": 2,
          "col": 9
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Only variables are allowed as left-hand side of let rec"
      },
      {
        "start": {
          "line": 6,
          "col": 13
        },
        "end": {
          "line": 6,
          "col": 18
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This constant has type string but an expression was expected of type int"
      }
    ],
    "notifications": []
  }
