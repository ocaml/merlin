  $ cat >test.ml <<EOF
  > let g (x : int) = x
  > let b = g 1
  > EOF

  $ $MERLIN single type-enclosing -position 2:8 \
  > -filename test.ml < test.ml |
  > jq '.value'
  [
    {
      "start": {
        "line": 2,
        "col": 8
      },
      "end": {
        "line": 2,
        "col": 9
      },
      "type": "int -> int",
      "tail": "no"
    },
    {
      "start": {
        "line": 2,
        "col": 8
      },
      "end": {
        "line": 2,
        "col": 11
      },
      "type": "int",
      "tail": "no"
    }
  ]
