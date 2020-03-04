Get the type of a module type with the same name as a module:

  $ $MERLIN single type-enclosing -position 5:9 -verbosity 0 \
  > -filename ./module_type.mli < ./module_type.mli | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 5,
        "col": 8
      },
      "end": {
        "line": 5,
        "col": 9
      },
      "type": "T",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 5:9 -verbosity 2 \
  > -filename ./module_type.mli < ./module_type.mli | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 5,
        "col": 8
      },
      "end": {
        "line": 5,
        "col": 9
      },
      "type": "sig type a end",
      "tail": "no"
    }
  ]
