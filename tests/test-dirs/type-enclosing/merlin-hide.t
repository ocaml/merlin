Make sure type-enclosing works properly even when the precise location is not
accessible:

  $ $MERLIN single type-enclosing -position 3:7 -filename hide.ml <<EOF
  > module M = struct
  >   include struct
  >     let x = 3
  >   end[@merlin.hide]
  > end
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 11
        },
        "end": {
          "line": 5,
          "col": 3
        },
        "type": "sig val x : int end",
        "tail": "no"
      },
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 5,
          "col": 3
        },
        "type": "sig val x : int end",
        "tail": "no"
      }
    ],
    "notifications": []
  }
