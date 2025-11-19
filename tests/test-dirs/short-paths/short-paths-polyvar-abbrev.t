  $ cat > sample.ml <<EOF
  > module C = struct
  >   type r = int
  >   type k = string
  > end
  > module A = struct
  >   type t = [\`Foo | \`Bar of int | \`Baz of string ]
  > end
  > type c = A.t
  > let f : A.t -> unit = function #A.t -> ()
  > EOF
  $ $MERLIN single type-enclosing -position 9:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  A.t -> unit

  $ $MERLIN single type-enclosing -short-paths -position 9:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  c -> unit


  $ cat > sample.ml <<EOF
  > module C = struct
  >   type r = int
  >   type k = string
  > end
  > module A = struct
  >   type t = [\`Foo | \`Bar of int | \`Baz of string ]
  > end
  > type c = [\`Foo | \`Bar of int | \`Baz of string ]
  > let f : A.t -> unit = function #A.t -> ()
  > EOF
  $ $MERLIN single type-enclosing -position 9:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  A.t -> unit

  $ $MERLIN single type-enclosing -short-paths -position 9:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  A.t -> unit

