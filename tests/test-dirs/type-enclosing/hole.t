Check that we can access the expected type of a hole:
  $ $MERLIN single type-enclosing -position 2:2 -filename hole.ml <<EOF
  > let f () : int =
  >   _
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 2
        },
        "end": {
          "line": 2,
          "col": 3
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
          "col": 3
        },
        "type": "unit -> int",
        "tail": "no"
      }
    ],
    "notifications": []
  }
