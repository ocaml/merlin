  $ cat >invalid_format.ml <<'EOF'
  > let _ = Format.asprintf "%d%\r" 10
  > let x = 10
  > let _ = Format.asprintf "%d%\e" 10
  > let _ : bool = 10
  > EOF

  $ $MERLIN single errors -filename invalid_format.ml < invalid_format.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 24
        },
        "end": {
          "line": 1,
          "col": 31
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "invalid format \"%d%\\r\": at character number 3, invalid conversion \"%\r\""
      },
      {
        "start": {
          "line": 3,
          "col": 24
        },
        "end": {
          "line": 3,
          "col": 31
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "invalid format \"%d%\\\\e\": at character number 3, invalid conversion \"%\\\""
      },
      {
        "start": {
          "line": 4,
          "col": 15
        },
        "end": {
          "line": 4,
          "col": 17
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The constant 10 has type int but an expression was expected of type bool"
      }
    ],
    "notifications": []
  }
