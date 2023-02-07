  $ cat >main.ml <<EOF
  > module Kind = struct
  >   type t =
  >     | A
  >     | B
  > end
  > type t = { kind : Kind.t }
  > let x = { kind = A }
  > let y = x.kind
  > let z = { kind = B }.kind
  > EOF

On `let |y = x.kind`
Verbosity 0 stops at Kind.t
  $ $MERLIN single type-enclosing -position 8:4 -verbosity 0 \
  > -filename ./main.ml < ./main.ml | tr '\r\n' ' ' | jq ".value[0].type"
  "Kind.t"

On `let |y = x.kind`
Verbosity 1 shows the actual type definition of Kind.t
  $ $MERLIN single type-enclosing -position 8:4 -verbosity 1 \
  > -filename ./main.ml < ./main.ml | tr '\r\n' ' ' | jq ".value[0].type"
  "type t = A | B"

On `let y = x.k|ind`
  $ $MERLIN single type-enclosing -position 8:11 -verbosity 0 \
  > -filename ./main.ml < ./main.ml | tr '\r\n' ' ' | jq ".value[0].type"
  "Kind.t"

On `let y = x.k|ind`
  $ $MERLIN single type-enclosing -position 8:11 -verbosity 1 \
  > -filename ./main.ml < ./main.ml | tr '\r\n' ' ' | jq ".value[0].type"
  "type t = A | B"

On `let z = { kind = B }.k|ind`
  $ $MERLIN single type-enclosing -position 9:22 -verbosity 0 \
  > -filename ./main.ml < ./main.ml | tr '\r\n' ' ' | jq ".value[0].type"
  "Kind.t"

On `let z = { kind = B }.k|ind`
  $ $MERLIN single type-enclosing -position 9:22 -verbosity 1 \
  > -filename ./main.ml < ./main.ml | tr '\r\n' ' ' | jq ".value[0].type"
  "type t = A | B"

On `let x = { k|ind = A }`
  $ $MERLIN single type-enclosing -position 7:12 -verbosity 0 \
  > -filename ./main.ml < ./main.ml | tr '\r\n' ' ' | jq ".value[0].type"
  "Kind.t"

On `let x = { k|ind = A }`
  $ $MERLIN single type-enclosing -position 7:12 -verbosity 1 \
  > -filename ./main.ml < ./main.ml | tr '\r\n' ' ' | jq ".value[0].type"
  "type t = A | B"
