(enabled_if (< %{ocaml_version} 4.06.0))

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
  > -log-file /tmp/mlog_pre406 -filename ./context.ml < ./context.ml | jq ".value[0:2]"
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
      "type": "'a -> 'a",
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
      "type": "'a -> 'a",
      "tail": "no"
    }
  ]
