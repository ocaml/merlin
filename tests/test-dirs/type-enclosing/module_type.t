Get the type of a module type with the same name as a module:

  $ $MERLIN single type-enclosing -position 6:8 -verbosity 0 \
  > -filename ./module_type.mli < ./module_type.mli | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 6,
        "col": 8
      },
      "end": {
        "line": 6,
        "col": 9
      },
      "type": "sig type b end",
      "tail": "no"
    },
    {
      "start": {
        "line": 6,
        "col": 8
      },
      "end": {
        "line": 6,
        "col": 9
      },
      "type": "T",
      "tail": "no"
    }
  ]
