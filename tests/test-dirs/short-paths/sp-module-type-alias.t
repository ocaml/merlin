Module type paths should be shortened as module types, not as modules.

We mimic a wrapped library where a module type is an abbreviation of a module
type defined in an internal compilation unit:

  $ cat >mylib__.ml <<'EOF'
  > module Per_item_intf = Mylib__Per_item_intf
  > EOF

  $ cat >per_item_intf.ml <<'EOF'
  > module type S = sig type t val x : t end
  > EOF

  $ cat >mylib.ml <<'EOF'
  > module type Per_item = Per_item_intf.S
  > module type Other = sig type u end
  > EOF

  $ $OCAMLC -c -no-alias-deps -w -49 mylib__.ml
  $ $OCAMLC -c -open Mylib__ -o mylib__Per_item_intf per_item_intf.ml
  $ $OCAMLC -c -open Mylib__ mylib.ml

  $ cat >test.ml <<'EOF'
  > include Mylib
  > module F (X : Per_item) = X
  > module G (X : Mylib.Per_item) = X
  > EOF

  $ cat >test2.ml <<'EOF'
  > open Mylib
  > module F (X : Per_item) = X
  > EOF

FIXME The definition of [Per_item] should not mention the internal [Mylib__]:
  $ $MERLIN single type-enclosing -short-paths -position 1:9 -index 0 \
  > -filename test.ml < test.ml | tr '\n' ' ' | jq -r '.value[0].type'
  sig   module type Per_item = Mylib__.Per_item_intf.S   module type Other = sig type u end end

  $ $MERLIN single type-enclosing -short-paths -position 2:7 -index 0 \
  > -filename test.ml < test.ml | tr '\n' ' ' | jq -r '.value[0].type'
  (X : Per_item) -> sig type t = X.t val x : t end

FIXME [Per_item] is in scope thanks to the open:
  $ $MERLIN single type-enclosing -short-paths -position 2:7 -index 0 \
  > -filename test2.ml < test2.ml | jq -r '.value[0].type'
  (X : Mylib.Per_item) -> sig type t = X.t val x : t end
