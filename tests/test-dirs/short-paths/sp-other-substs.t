  $ cat >quite_long__name.ml <<'EOF'
  > module M = struct type t = T let x = T end 
  > EOF

  $ cat >import0.ml <<'EOF'
  > module N = Quite_long__name.M
  > EOF

  $ cat >import__.ml <<'EOF'
  > include Import0
  > EOF

  $ cat >base__.ml <<'EOF'
  > module Import = Import__
  > EOF

  $ cat >test.ml <<'EOF'
  > open Import;;
  > ignore N.x
  > EOF

  $ $OCAMLC -c quite_long__name.ml import0.ml import__.ml base__.ml

  $ echo "FLG -short-paths -open Base__" > .merlin

Should take the alias in Import into account. Should be N.t.
  $ $MERLIN single type-enclosing -position 2:9 \
  > -filename test.ml < test.ml | jq '.value[0].type'
  "N.t"
