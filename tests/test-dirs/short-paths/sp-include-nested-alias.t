Including a module path should bring its components in U

  $ cat >mylib__.ml <<'EOF'
  > module Compact_position = Mylib__Compact_position
  > EOF

  $ cat >compact_position.ml <<'EOF'
  > type t
  > EOF

  $ cat >mylib.ml <<'EOF'
  > module For_tests = struct
  >   module Compact_position = Compact_position
  > end
  > EOF

  $ $OCAMLC -c -no-alias-deps -w -49 mylib__.ml
  $ $OCAMLC -c -open Mylib__ -o mylib__Compact_position compact_position.ml
  $ $OCAMLC -c -open Mylib__ mylib.ml

  $ cat >test_open.ml <<'EOF'
  > open Mylib
  > EOF

  $ cat >test_include.ml <<'EOF'
  > include Mylib
  > EOF

  $ $MERLIN single type-enclosing -short-paths -position 1:9 -index 0 \
  > -filename test_open.ml < test_open.ml | tr '\n' ' ' | jq -r '.value[0].type'
  sig   module For_tests :     sig module Compact_position = Mylib.For_tests.Compact_position end end

This should also be [Mylib.For_tests.Compact_position]:
  $ $MERLIN single type-enclosing -short-paths -position 1:9 -index 0 \
  > -filename test_include.ml < test_include.ml | tr '\n' ' ' | jq -r '.value[0].type'
  sig   module For_tests :     sig module Compact_position = Mylib.For_tests.Compact_position end end

  $ cat >s1.mli <<'EOF'
  > include module type of Mylib
  > val x : Mylib.For_tests.Compact_position.t
  > EOF

This should also be [Mylib.For_tests.Compact_position]:
  $ $MERLIN single type-enclosing -short-paths -position 1:24 -index 0 \
  > -filename s1.mli < s1.mli | tr '\n' ' ' | jq -r '.value[0].type'
  sig   module For_tests :     sig module Compact_position = Mylib.For_tests.Compact_position end end

  $ $MERLIN single type-enclosing -short-paths -position 2:4 -index 0 \
  > -filename s1.mli < s1.mli | tr '\n' ' ' | jq -r '.value[0].type'
  For_tests.Compact_position.t

  $ cat >s2.mli <<'EOF'
  > module type S = module type of Mylib
  > include S
  > EOF

FIXME This should also be [Mylib.For_tests.Compact_position]:
  $ $MERLIN single type-enclosing -short-paths -position 2:9 -index 0 \
  > -filename s2.mli < s2.mli | tr '\n' ' ' | jq -r '.value[0].type'
  sig   module For_tests :     sig module Compact_position = Mylib__Compact_position end end
