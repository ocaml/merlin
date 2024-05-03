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
        "type": "parser",
        "sub": [],
        "valid": true,
        "message": "In this scoped type, variable 'a is reserved for the local type a."
      }
    ],
    "notifications": []
  }

Merlin would fail to catch this new exception:
"In this scoped type, variable 'a is reserved for the local type a.",
