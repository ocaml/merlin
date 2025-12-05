  $ cat >main.ml <<EOF
  > let foo a b = b
  > EOF

  $ $MERLIN single type-enclosing -position 1:6 -filename main.ml <main.ml | jq .value.[0].type
  "'a -> 'b -> 'b"

Inconsistent type variables are returned by `type-enclosing`. In term of user experience, the type of `b` it should be `'b`.
See https://github.com/ocaml/merlin/issues/1949.
  $ $MERLIN single type-enclosing -position 1:14 -filename main.ml <main.ml | jq .value.[0].type
  "'a"
