Lib_a is a wrapped library (dune-style) that defines Header with type t.
Lib_b re-exports Header and uses it in a record.
Import provides both Header directly and State = Lib_b.
Short-paths should prefer Header.t but prints Lib_b.Header.t.

  $ mkdir lib_a
  $ mkdir lib_b

  $ cd lib_a
  $ cat > lib_a__.ml-gen << 'EOF'
  > module Header = Lib_a__Header
  > EOF

  $ cat >header.mli <<'EOF'
  > type t
  > EOF

  $ cat > lib_a.ml << 'EOF'
  > module Header = Header
  > EOF

  $ $OCAMLC -w -49 -no-alias-deps -o lib_a__.cmo -c -impl lib_a__.ml-gen
  $ $OCAMLC -o lib_a__Header.cmi -c -intf header.mli -open Lib_a__
  $ $OCAMLC -o lib_a.cmi -c -impl lib_a.ml -open Lib_a__

  $ cd ../lib_b

  $ cat >lib_b.ml <<'EOF'
  > module Header = Lib_a.Header
  > type t = { header : Header.t }
  > EOF

  $ $OCAMLC -o lib_b.cmi -c -impl lib_b.ml -I ../lib_a
  $ ocamlobjinfo -quiet -discourse lib_b.cmi
  Discourse:
  Header: alias: Lib_a.Header [Lib_a!.Header] Lib_a!.Header
  
  t: Header/281.t

  $ cd ..

  $ cat >import.ml << 'EOF'
  > module Header = Lib_a.Header
  > module State = Lib_b
  > EOF

  $ $OCAMLC -o import.cmi -c -impl import.ml -I lib_a -I lib_b

  $ cat >usage.ml << 'EOF'
  > open Import
  > let f (t : State.t) = t.header
  > EOF

  $ cat >.merlin <<'EOF'
  > FLG -short-paths
  > B .
  > B lib_a
  > B lib_b
  > EOF

  $ $MERLIN single type-enclosing -position 2:27 -index 0 \
  > -filename usage.ml <usage.ml | jq '.value[0].type'
  "Header.t"
