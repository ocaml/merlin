  $ cat > test.ml <<EOF
  > type t =
  > A | B
  > let f = Fun.id
  > EOF

  $ $MERLIN single locate -position 3:0 -prefix List.hd -look-for interface -filename test.ml < test.ml | jq .value | jq .pos
  {
    "line": 76,
    "col": 4
  }

-- FIXME : locate should also points to the list interface file. Merlin's environment before the end of the first structure item is Empty when it should be the initial env.
  $ $MERLIN single locate -position 2:0 -prefix List.hd -look-for interface -filename test.ml < test.ml | jq .value
  "Already at definition point"
