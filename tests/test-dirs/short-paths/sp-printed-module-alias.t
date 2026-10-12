The items of a printed signature are in scope for the items printed after
them: they are candidates for their names.

The library mimics dune's [Dune_digest]: its main module re-exports an
internal unit with an include.

  $ cat >mylib__.ml <<'EOF'
  > module Impl = Mylib__Impl
  > EOF

  $ cat >impl.ml <<'EOF'
  > module Sub = struct type t = int end
  > EOF

  $ cat >mylib.ml <<'EOF'
  > module Impl = Impl
  > include Impl
  > EOF

  $ $OCAMLC -c -no-alias-deps -w -49 mylib__.ml
  $ $OCAMLC -c -open Mylib__ -o mylib__Impl impl.ml
  $ $OCAMLC -c -open Mylib__ mylib.ml

  $ cat >test.ml <<'EOF'
  > module M = Mylib
  > EOF

[Sub] is an alias of [Mylib__Impl.Sub], and [Impl], printed before it, is an
alias of [Mylib__Impl]. [Sub] should be printed as [Impl.Sub]:
  $ $MERLIN single type-enclosing -short-paths -position 1:13 -index 0 \
  > -filename test.ml < test.ml | tr '\n' ' ' | jq -r '.value[0].type'
  sig module Impl = Mylib.Impl module Sub = Impl.Sub end

  $ $MERLIN single type-enclosing -short-paths -legacy-short-path \
  > -position 1:13 -index 0 \
  > -filename test.ml < test.ml | tr '\n' ' ' | jq -r '.value[0].type'
  sig module Impl = Mylib.Impl module Sub = Mylib.Sub end

The same when the typed node of the alias is printed:
  $ $MERLIN single type-enclosing -short-paths -verbosity smart \
  > -position 1:13 -index 1 \
  > -filename test.ml < test.ml | tr '\n' ' ' | jq -r '.value[1].type'
  sig module Impl = Mylib.Impl module Sub = Impl.Sub end
