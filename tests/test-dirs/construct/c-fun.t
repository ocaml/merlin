  $ cat >fun1.ml <<EOF
  > module Mymod = struct type the_type = int end
  > type the_type = float
  > let x : Mymod.the_type -> the_type -> unit =
  >   _
  > EOF

  $ $MERLIN single construct -position 4:2 \
  > -filename fun1.ml <fun1.ml | jq ".value"
  [
    {
      "start": {
        "line": 4,
        "col": 2
      },
      "end": {
        "line": 4,
        "col": 3
      }
    },
    [
      "(fun the_type the_type_1 -> _)"
    ]
  ]

  $ cat >fun2.ml <<EOF
  > module Mymod = struct type int = string end
  > type int = float
  > let x : Mymod.int -> int -> unit =
  >   _
  > EOF

  $ $MERLIN single construct -position 4:2 \
  > -filename fun2.ml <fun2.ml | jq ".value"
  [
    {
      "start": {
        "line": 4,
        "col": 2
      },
      "end": {
        "line": 4,
        "col": 3
      }
    },
    [
      "(fun int int_1 -> _)"
    ]
  ]
