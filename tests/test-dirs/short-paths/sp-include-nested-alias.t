Including a module path should bring its components in U

  $ cat >mylib__.ml <<'EOF'
  > module Compact_position = Mylib__Compact_position
  > EOF

  $ cat >compact_position.ml <<'EOF'
  > type t = int
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

FIXME This should also be [Mylib.For_tests.Compact_position]:
  $ $MERLIN single type-enclosing -short-paths -position 1:9 -index 0 \
  > -filename test_include.ml < test_include.ml | tr '\n' ' ' | jq -r '.value[0].type'
  sig   module For_tests :     sig module Compact_position = Mylib__Compact_position end end
