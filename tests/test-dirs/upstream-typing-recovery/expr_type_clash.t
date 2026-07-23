  $ cat >expr_type_clash.ml <<'EOF'
  > let f (x : int) : int = x + 1
  > 
  > let _ = f "hello"
  > 
  > 
  > let _ =
  >   (1 : string)
  > 
  > let g (x : string) = x ^ "!"
  > 
  > let _ =
  >   let y = g 123 in
  >   y
  > EOF

  $ $MERLIN single errors -filename expr_type_clash.ml < expr_type_clash.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 10
        },
        "end": {
          "line": 3,
          "col": 17
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This constant has type string but an expression was expected of type int"
      },
      {
        "start": {
          "line": 7,
          "col": 3
        },
        "end": {
          "line": 7,
          "col": 4
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The constant 1 has type int but an expression was expected of type string"
      },
      {
        "start": {
          "line": 12,
          "col": 12
        },
        "end": {
          "line": 12,
          "col": 15
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The constant 123 has type int but an expression was expected of type string"
      }
    ],
    "notifications": []
  }
