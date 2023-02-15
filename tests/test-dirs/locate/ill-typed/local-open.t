  $ cat >open.ml <<EOF
  > let x = false
  > let f () =
  >   let open Unknown in
  >   x
  > ;;
  > EOF

  $ $MERLIN single locate -position 4:2 \
  > -filename open.ml <open.ml | jq '.value.pos'
  {
    "line": 1,
    "col": 4
  }
