This test shows a possible mixup between module whose type is a module type path
and module type aliases.

  $ cat >foo.ml <<'EOF'
  > module type S = sig type t val x : t end
  > module S : S = struct type t = int let x = 0 end
  > EOF

  $ $OCAMLC -c foo.ml 

  $ cat >test.ml <<'EOF'
  > module M : Foo.S = struct type t = string let x = "" end
  > let y = Foo.S.x
  > EOF

  $ cat >test2.ml <<'EOF'
  > module Long = struct
  >   module type S = sig type t val x : t end
  >   module S : S = struct type t = int let x = 0 end
  > end
  > module M : Long.S = struct type t = string let x = "" end
  > let y = Long.S.x
  > let z = M.x
  > EOF


FIXME [M.t] is a different abstract type, this should be [Foo.S.t]:
  $ $MERLIN single type-enclosing -short-paths -position 2:4 -index 0 \
  > -filename test.ml < test.ml | jq -r '.value[0].type'
  M.t

FIXME Should be [Long.S.t]:
  $ $MERLIN single type-enclosing -short-paths -position 6:4 -index 0 \
  > -filename test2.ml < test2.ml | jq -r '.value[0].type'
  M.t

Should be [M.t]:
  $ $MERLIN single type-enclosing -short-paths -position 7:4 -index 0 \
  > -filename test2.ml < test2.ml | jq -r '.value[0].type'
  M.t
