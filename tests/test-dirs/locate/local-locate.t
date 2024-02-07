  $ cat >main.ml <<EOF
  > let _ = let x = 42 in x
  > EOF

  $ $MERLIN single locate -look-for ml -position 1:22 \
  > -filename main.ml <main.ml | jq '.value.pos'
  {
    "line": 1,
    "col": 12
  }

  $ $MERLIN single locate -look-for mli -position 1:22 \
  > -filename main.ml <main.ml | jq '.value.pos'
  {
    "line": 1,
    "col": 12
  }
