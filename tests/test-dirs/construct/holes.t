  $ cat >h1.ml <<EOF
  > EOF

  $ $MERLIN single holes -filename h1.ml <h1.ml |
  >  jq ".value"
  []


  $ cat >h2.ml <<EOF
  > let x : int option = _
  > let g x y = x * y
  > let f x y = g _ _
  > module M : sig val f : int -> unit end = _
  > EOF

  $ $MERLIN single holes -filename h2.ml <h2.ml |
  >  jq ".value"
  [
    {
      "start": {
        "line": 1,
        "col": 21
      },
      "end": {
        "line": 1,
        "col": 22
      },
      "type": "int option"
    },
    {
      "start": {
        "line": 3,
        "col": 14
      },
      "end": {
        "line": 3,
        "col": 15
      },
      "type": "int"
    },
    {
      "start": {
        "line": 3,
        "col": 16
      },
      "end": {
        "line": 3,
        "col": 17
      },
      "type": "int"
    },
    {
      "start": {
        "line": 4,
        "col": 41
      },
      "end": {
        "line": 4,
        "col": 42
      },
      "type": "sig val f : int -> unit end"
    }
  ]
