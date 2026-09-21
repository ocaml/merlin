  $ cat >import.ml <<'EOF'
  > module A = struct module B = struct type t = T end end
  > type u = A.B.t
  > EOF

  $ cat >test.ml <<'EOF'
  > let x : A.B.t = T;;
  > ignore x
  > EOF

  $ cat >dune <<'EOF'
  > (library
  >  (name test)
  >  (flags :standard -open Import)
  >  (libraries import))
  > EOF

  $ $OCAMLC -c import.ml
  $ $OCAMLC -c test.ml -open Import 

  $ cat >.merlin <<'EOF'
  > FLG -short-paths -open Import
  > EOF

The shortest path is `u`
  $ $MERLIN single type-enclosing -position 2:7 \
  > -filename test.ml < test.ml | jq '.value[0].type'
  "u"
