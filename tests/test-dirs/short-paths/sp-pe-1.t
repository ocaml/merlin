  $ cat >import.ml <<'EOF'
  > module A = struct
  >   module B = struct
  >     type t = X
  >   end
  > end
  > 
  > type t = A.B.t
  > 
  > type c = A of t
  > EOF

  $ $OCAMLC -o import.cmi -c -impl import.ml

  $ cat >main.ml <<'EOF'
  > let y = Import.A X
  > EOF

  $ $OCAMLC -o main.cmi -c -impl main.ml

  $ cat >.merlin <<'EOF'
  > FLG -short-paths -nostdlib
  > B .
  > EOF

FIXME we expect Import.t
  $ $MERLIN single type-enclosing -position 1:17 -index 0 \
  > -filename main.ml <main.ml | jq '.value[0].type'
  "Import.t"
