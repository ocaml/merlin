Fixed.

  $ $MERLIN single type-enclosing  -position 5:9 -filename test.ml <<EOF
  > module Foo = struct
  >   let bar = 42
  > end
  > type t = Foo of int
  > let a = Foo 3
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 5,
          "col": 8
        },
        "end": {
          "line": 5,
          "col": 11
        },
        "type": "int -> t",
        "tail": "no"
      },
      {
        "start": {
          "line": 5,
          "col": 8
        },
        "end": {
          "line": 5,
          "col": 13
        },
        "type": "t",
        "tail": "no"
      }
    ],
    "notifications": []
  }
