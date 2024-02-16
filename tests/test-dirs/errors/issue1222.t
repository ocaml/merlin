FIXME UPGRADE 5.2: The first error is still a parser error... Following errors
should be ignored ?
  $ $MERLIN single errors -filename issue1222.ml <<EOF
  > let minimal : type a. 'a t
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 22
        },
        "end": {
          "line": 1,
          "col": 24
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "In this scoped type, variable 'a is reserved for the local type a."
      },
      {
        "start": {
          "line": 1,
          "col": 25
        },
        "end": {
          "line": 1,
          "col": 26
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound type constructor t"
      },
      {
        "start": {
          "line": 2,
          "col": 0
        },
        "end": {
          "line": 2,
          "col": 0
        },
        "type": "parser",
        "sub": [],
        "valid": true,
        "message": "Syntax error, expecting `='"
      }
    ],
    "notifications": []
  }

Merlin would fail to catch this new exception:
"In this scoped type, variable 'a is reserved for the local type a.",
