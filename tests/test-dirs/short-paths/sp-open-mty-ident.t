Opening a module whose type is a module type path should bring its components
in scope.

  $ cat >test.ml <<'EOF'
  > module type S = sig type t val x : t end
  > module M : S = struct type t = int let x = 0 end
  > open M
  > let y = x
  > EOF

FIXME This should be [t]:
  $ $MERLIN single type-enclosing -short-paths -position 4:4 -index 0 \
  > -filename test.ml < test.ml | jq -r '.value[0].type'
  t
