  $ cat >main.ml <<EOF
  > let _ = let x = 42 in x
  > EOF

  $ export LOC=1:22
  $ show_location main.ml $LOC
  let _ = let x = 42 in █
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
