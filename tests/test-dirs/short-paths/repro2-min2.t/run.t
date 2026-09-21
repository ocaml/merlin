Files
=====

| priv/priv__a.mli : type t
| priv/priv__b.mli : val f : Priv__a.t -> int
| priv/layer.ml    : module A = Priv__a
|                    module B = Priv__b
| m.ml             : open Layer
|                    module A = A
|                    module B = B
| usage.ml         : let _ = M.B.f

From usage.ml the type of M.B.f in usage.ml can be described as:

- Priv__a.t (but this has a bad score due to the __)
- M.A.t

So `M.A.t` is the clearly preferred candidate — *if* it's in D. The bug is that
it isn't.

Build the priv library (a, b, layer all in priv/).
  $ mkdir -p priv

  $ cat > priv/priv__a.mli << 'EOF'
  > type t
  > EOF
  $ cat > priv/priv__b.mli << 'EOF'
  > val f : Priv__a.t -> int
  > EOF

  $ $OCAMLC -I priv -c -intf priv/priv__a.mli
  $ $OCAMLC -I priv -c -intf priv/priv__b.mli

  $ cat > priv/layer.ml << 'EOF'
  > module A = Priv__a
  > module B = Priv__b
  > EOF

  $ $OCAMLC -I priv -c -impl priv/layer.ml

Build main.
  $ cat > m.ml << 'EOF'
  > open Layer
  > module A = A
  > module B = B
  > EOF

  $ $OCAMLC -I priv -c -impl m.ml

Usage.ml
  $ cat > usage.ml << 'EOF'
  > let _ = M.B.f
  > EOF

  $ cat > .merlin << 'EOF'
  > FLG -short-paths -nostdlib
  > B .
  > BH priv
  > EOF

Expected: "M.A.t -> int".
  $ $MERLIN single type-enclosing -position 1:13 -index 0 \
  > -filename usage.ml < usage.ml | jq '.value[0].type'
  "M.A.t -> int"
