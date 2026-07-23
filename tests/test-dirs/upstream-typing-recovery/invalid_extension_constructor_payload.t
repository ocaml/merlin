  $ cat >invalid_extension_constructor_payload.ml <<'EOF'
  > let x = [%extension_constructor]
  > let f =
  >   let a = [%extension_constructor]
  >   and b = [%extension_constructor] in
  >   (a, b)
  > 
  > let u = (snd f, fst f, x)
  > let t : int = u
  > EOF

  $ $MERLIN single errors -filename invalid_extension_constructor_payload.ml < invalid_extension_constructor_payload.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 8
        },
        "end": {
          "line": 1,
          "col": 32
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Invalid [%extension_constructor] payload, a constructor is expected."
      },
      {
        "start": {
          "line": 3,
          "col": 10
        },
        "end": {
          "line": 3,
          "col": 34
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Invalid [%extension_constructor] payload, a constructor is expected."
      },
      {
        "start": {
          "line": 4,
          "col": 10
        },
        "end": {
          "line": 4,
          "col": 34
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Invalid [%extension_constructor] payload, a constructor is expected."
      },
      {
        "start": {
          "line": 8,
          "col": 14
        },
        "end": {
          "line": 8,
          "col": 15
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The value u has type 'a * 'b * 'c but an expression was expected of type int"
      }
    ],
    "notifications": []
  }
