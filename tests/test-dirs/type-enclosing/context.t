TODO: these test are incorect !
should be solved with better context usage

  $ $MERLIN single type-enclosing -position 5:11 -verbosity 0 \
  > -filename ./context.ml < ./context.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 5,
        "col": 10
      },
      "end": {
        "line": 5,
        "col": 11
      },
      "type": "type x = Foo",
      "tail": "no"
    },
    {
      "start": {
        "line": 5,
        "col": 10
      },
      "end": {
        "line": 5,
        "col": 11
      },
      "type": "x",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 11:12 -verbosity 0 \
  > -filename ./context.ml < ./context.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 11,
        "col": 10
      },
      "end": {
        "line": 11,
        "col": 14
      },
      "type": "type unit = ()",
      "tail": "no"
    },
    {
      "start": {
        "line": 11,
        "col": 10
      },
      "end": {
        "line": 11,
        "col": 14
      },
      "type": "'a -> 'b",
      "tail": "no"
    }
  ]
