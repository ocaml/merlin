  $ cat >apply_non_function.ml <<'EOF'
  > let x = 42
  > 
  > let _ = x 1
  > 
  > let y = 10
  > 
  > let _ = y 2
  > EOF

  $ $MERLIN single errors -filename apply_non_function.ml < apply_non_function.ml
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
          "col": 9
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type int
  This is not a function; it cannot be applied."
      },
      {
        "start": {
          "line": 7,
          "col": 8
        },
        "end": {
          "line": 7,
          "col": 9
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type int
  This is not a function; it cannot be applied."
      }
    ],
    "notifications": []
  }
