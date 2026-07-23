  $ cat >illegal_value_name.ml <<'EOF'
  > let x =
  >   let (~##) x = x + 1 and (#~#) x y = x + y in
  >   (~##10) #~# 10
  > EOF

  $ $MERLIN single errors -filename illegal_value_name.ml < illegal_value_name.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 6
        },
        "end": {
          "line": 2,
          "col": 11
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "~## is not a valid value identifier."
      },
      {
        "start": {
          "line": 2,
          "col": 26
        },
        "end": {
          "line": 2,
          "col": 31
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "#~# is not a valid value identifier."
      },
      {
        "start": {
          "line": 3,
          "col": 3
        },
        "end": {
          "line": 3,
          "col": 6
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "~## is not a valid value identifier."
      },
      {
        "start": {
          "line": 3,
          "col": 10
        },
        "end": {
          "line": 3,
          "col": 13
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "#~# is not a valid value identifier."
      }
    ],
    "notifications": []
  }
