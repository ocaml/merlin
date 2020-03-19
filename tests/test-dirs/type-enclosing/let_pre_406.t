(enabled_if (< %{ocaml_version} 4.06.0))

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
      "type": "float",
      "tail": "no"
    }
  ]

  $ cat log
  # 0.01 type-enclosing - from_nodes
  unhandled node under cursor: value_binding
  # 0.01 type-enclosing - from_nodes
  unhandled node under cursor: structure_item
  # 0.01 type-enclosing - from_nodes
  unhandled node under cursor: structure
  # 0.01 type-enclosing - reconstruct identifier
  [
    {
      "start": { "line": 4, "col": 4 },
      "end": { "line": 4, "col": 7 },
      "identifier": "def"
    }
  ]
  # 0.01 type-enclosing - from_reconstructed
  node = pattern (let.ml[4,14+4]..let.ml[4,14+7])
    Tpat_var "def/1217"
  # 0.01 type-enclosing - from_reconstructed
  skipping lident
  # 0.01 type-enclosing - small enclosing
  result = [  ]
