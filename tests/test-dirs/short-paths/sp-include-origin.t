The types copied by [include P] record the type they were copied from, so that
a copy and its original share the same canonical path even when the copy, an
abbreviation, cannot be normalized to the original. This works with several
includes, and does not link a type that merely has the same expansion.

  $ cat >first_unit.ml <<'EOF'
  > type 'a t = 'a option list
  > EOF

  $ cat >second_unit.ml <<'EOF'
  > type 'a u = ('a * 'a) list
  > EOF

  $ cat >c.ml <<'EOF'
  > module M = struct
  >   include First_unit
  >   include Second_unit
  >   type 'a v = 'a option list
  > end
  > EOF

  $ $OCAMLC -c first_unit.ml second_unit.ml c.ml

  $ cat >main.ml <<'EOF'
  > open C
  > let x : int First_unit.t = []
  > let y : int Second_unit.u = []
  > let z : int M.v = []
  > EOF

  $ cat >.merlin <<'EOF'
  > FLG -short-paths
  > B .
  > S .
  > EOF

The origin of each copy is stored in the cmi
  $ ocamlobjinfo -quiet -discourse c.cmi
  Discourse:
  M: 
  M.t: 
  M.u: 
  M.v: 

We expect M.t, the copy of First_unit.t
  $ $MERLIN single type-enclosing -position 2:5 \
  > -filename main.ml < main.ml | jq '.value[0].type'
  "int First_unit.t"

We expect M.u, the copy of Second_unit.u
  $ $MERLIN single type-enclosing -position 3:5 \
  > -filename main.ml < main.ml | jq '.value[0].type'
  "int Second_unit.u"

M.v is not a copy, it stays M.v
  $ $MERLIN single type-enclosing -position 4:5 \
  > -filename main.ml < main.ml | jq '.value[0].type'
  "int M.v"
