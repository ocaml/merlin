Definitions in a signature should be candidates, as they are in a structure.

  $ cat >long.ml <<'EOF'
  > module Path : sig type u end = struct type u = int end
  > EOF

  $ cat >b.ml <<'EOF'
  > type t = Long.Path.u
  > let f (x : Long.Path.u) = x
  > EOF

  $ cat >b.mli <<'EOF'
  > type t = Long.Path.u
  > val f : Long.Path.u -> Long.Path.u
  > EOF

  $ $OCAMLC -c long.ml

In the implementation:
  $ $MERLIN single type-enclosing -short-paths -position 2:4 -index 0 \
  > -filename b.ml < b.ml | jq -r '.value[0].type'
  t -> t

In the interface, this should also be [t -> t]:
  $ $MERLIN single type-enclosing -short-paths -position 2:4 -index 0 \
  > -filename b.mli < b.mli | jq -r '.value[0].type'
  t -> t
