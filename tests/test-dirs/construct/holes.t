  $ cat >h1.ml <<EOF
  > EOF

  $ $MERLIN single holes -filename h1.ml <h1.ml |
  >  jq ".value"
  []


  $ cat >h2.ml <<EOF
  > let x : int option = _
  > let f x y = g _ _
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
      }
    },
    {
      "start": {
        "line": 2,
        "col": 14
      },
      "end": {
        "line": 2,
        "col": 15
      }
    },
    {
      "start": {
        "line": 2,
        "col": 16
      },
      "end": {
        "line": 2,
        "col": 17
      }
    }
  ]
