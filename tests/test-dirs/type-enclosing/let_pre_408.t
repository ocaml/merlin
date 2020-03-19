(enabled_if (< %{ocaml_version} 4.08.0))

Get type of a shadowing let binding:

  $ $MERLIN single type-enclosing -position 4:4 -verbosity 0 \
  > -log-file log -log-section type-enclosing -filename ./let.ml < ./let.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 4,
        "col": 4
      },
      "end": {
        "line": 4,
        "col": 7
      },
      "type": "int",
      "tail": "no"
    },
    {
      "start": {
        "line": 4,
        "col": 4
      },
      "end": {
        "line": 4,
        "col": 34
      },
      "type": "float",
      "tail": "no"
    }
  ]

  $ cat log
  # 0.00 type-enclosing - from_nodes
  unhandled node under cursor: value_binding
  # 0.00 type-enclosing - from_nodes
  unhandled node under cursor: structure_item
  # 0.00 type-enclosing - from_nodes
  unhandled node under cursor: structure
  # 0.00 type-enclosing - reconstruct identifier
  [
    {
      "start": { "line": 4, "col": 4 },
      "end": { "line": 4, "col": 7 },
      "identifier": "def"
    }
  ]
  # 0.00 type-enclosing - from_reconstructed
  node = core_type
  # 0.00 type-enclosing - from_reconstructed
  typed def
  # 0.00 type-enclosing - small enclosing
  result = [ File "let.ml", line 4, characters 4-7 ]
