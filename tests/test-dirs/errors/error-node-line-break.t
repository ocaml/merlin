  $ cat >test.ml <<EOF
  > [%%ocaml.error "test with several words"]
  > EOF

  $ $MERLIN single errors -filename test.ml <test.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 3
        },
        "end": {
          "line": 1,
          "col": 14
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "test with several words"
      }
    ],
    "notifications": []
  }
