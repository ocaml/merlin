Check that we can access the expected type of a hole: (BROKEN)

  $ $MERLIN single type-enclosing -position 2:4 -filename hole.ml <<EOF
  > let f () : int =
  >   (??)
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 3
        },
        "end": {
          "line": 2,
          "col": 5
        },
        "type": "'a",
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 2
        },
        "end": {
          "line": 2,
          "col": 6
        },
        "type": "int",
        "tail": "no"
      },
      {
        "start": {
          "line": 1,
          "col": 6
        },
        "end": {
          "line": 2,
          "col": 6
        },
        "type": "unit -> int",
        "tail": "no"
      }
    ],
    "notifications": []
  }
