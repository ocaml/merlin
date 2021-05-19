  $ $MERLIN single type-enclosing -position 2:13 -verbosity 0 \
  > -filename ./issue1116.ml < ./issue1116.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 2,
        "col": 8
      },
      "end": {
        "line": 2,
        "col": 18
      },
      "type": "string",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 1:16 -verbosity 0 \
  > -filename ./issue1116.ml < ./issue1116.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 1,
        "col": 15
      },
      "end": {
        "line": 1,
        "col": 16
      },
      "type": "int",
      "tail": "no"
    }
  ]
