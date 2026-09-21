Mimic the dune build of the sources in this test sub directories as we did
for repro2.t/run.t.

The project has four dune libraries:

Build the repro4_priv library (no dependencies).
  $ cat > priv/repro4_priv__.ml-gen << 'EOF'
  > (** @canonical Repro4_priv.Kind *)
  > module Kind = Repro4_priv__Kind
  > EOF

  $ $OCAMLC -w -49 -no-alias-deps -o priv/repro4_priv__.cmo \
  >   -c -impl priv/repro4_priv__.ml-gen
  $ $OCAMLC -I priv -open Repro4_priv__ \
  >   -o priv/repro4_priv__Kind.cmo -c -impl priv/kind.ml
  $ $OCAMLC -I priv -open Repro4_priv__ \
  >   -o priv/repro4_priv.cmo -c -impl priv/repro4_priv.ml

Build the repro4_other library (no dependencies).
  $ cat > other/repro4_other__.ml-gen << 'EOF'
  > (** @canonical Repro4_other.Kind *)
  > module Kind = Repro4_other__Kind
  > EOF

  $ $OCAMLC -w -49 -no-alias-deps -o other/repro4_other__.cmo \
  >   -c -impl other/repro4_other__.ml-gen
  $ $OCAMLC -I other -open Repro4_other__ \
  >   -o other/repro4_other__Kind.cmo -c -impl other/kind.ml
  $ $OCAMLC -I other -open Repro4_other__ \
  >   -o other/repro4_other.cmo -c -impl other/repro4_other.ml

Build the repro4 library (depends on repro4_priv and repro4_other).
  $ cat > src/repro4__.ml-gen << 'EOF'
  > (** @canonical Repro4.Foo *)
  > module Foo = Repro4__Foo
  > 
  > (** @canonical Repro4.Kind *)
  > module Kind = Repro4__Kind
  > EOF

  $ $OCAMLC -w -49 -no-alias-deps -o src/repro4__.cmo \
  >   -c -impl src/repro4__.ml-gen
  $ $OCAMLC -I src -I priv -I other -open Repro4__ \
  >   -o src/repro4__Kind.cmi -c -intf src/kind.mli
  $ $OCAMLC -I src -I priv -I other -open Repro4__ \
  >   -o src/repro4__Foo.cmi -c -intf src/foo.mli
  $ $OCAMLC -I src -I priv -I other -open Repro4__ \
  >   -o src/repro4.cmo -c -impl src/repro4.ml

Create a .merlin in usage/ mirroring what dune would generate.
  $ cat > usage/.merlin << 'EOF'
  > FLG -short-paths -nostdlib
  > B .
  > BH ../priv
  > BH ../other
  > B ../src
  > EOF

Then the following test should return Repro4__.Kind.t but Repro4.Kind.t is not
the same type. There is no good public name for this type.
  $ $MERLIN single type-enclosing -position 1:34 -index 0 \
  > -filename usage/usage.ml <usage/usage.ml | jq '.value[0].type'
  "'a Repro4__.Kind.t"
