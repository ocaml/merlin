This test is for testing the behavior of identifiers with a . in them:

  $ cat >main.ml <<EOF
  > module A = struct let (+.) a b = a +. b end
  > let f x = A.(x +. 1.)
  > let g x = A.(+.) x 1.
  > EOF

  $ $MERLIN single locate -look-for ml -position 2:16 \
  > -filename ./main.ml < ./main.ml | jq '.value.pos'
  {
    "line": 1,
    "col": 22
  }

  $ $MERLIN single locate -look-for ml -position 3:14 \
  > -filename ./main.ml < ./main.ml | jq '.value.pos'
  {
    "line": 1,
    "col": 22
  }
