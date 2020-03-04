Various parts of the record.ml:

  $ $MERLIN single type-enclosing -position 4:11 -verbosity 0 \
  > -filename ./record.ml < ./record.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 4,
        "col": 10
      },
      "end": {
        "line": 4,
        "col": 11
      },
      "type": "float",
      "tail": "no"
    },
    {
      "start": {
        "line": 4,
        "col": 8
      },
      "end": {
        "line": 4,
        "col": 18
      },
      "type": "t",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 6:11 -verbosity 0 \
  > -log-file /tmp/m_log -filename ./record.ml < ./record.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 6,
        "col": 8
      },
      "end": {
        "line": 6,
        "col": 18
      },
      "type": "'a",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 8:11 -verbosity 0 \
  > -filename ./record.ml < ./record.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 8,
        "col": 10
      },
      "end": {
        "line": 8,
        "col": 11
      },
      "type": "float",
      "tail": "no"
    },
    {
      "start": {
        "line": 8,
        "col": 8
      },
      "end": {
        "line": 8,
        "col": 17
      },
      "type": "unit",
      "tail": "no"
    }
  ]
