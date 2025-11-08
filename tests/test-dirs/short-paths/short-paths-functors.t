  $ echo "" > .merlin
  > cat > sample.ml <<EOF
  > module F (D: sig type t val from_int : int -> t end) : sig
  >   type t
  >   val make : int -> t
  > end = struct
  >   type t = D.t
  >   let make x = D.from_int x
  > end
  > module A = F (struct type t = int let from_int x = x)
  > module B = F (struct type t = int let from_int x = x)
  > open A
  > let f x = B.make (A.make x)
  > EOF
  $ $MERLIN single type-enclosing -position 11:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  int -> B.t

  $ echo "FLG -short-paths" > .merlin
  > cat > sample.ml <<EOF
  > module F (D: sig type t val from_int : int -> t end) : sig
  >   type t
  >   val make : int -> t
  > end = struct
  >   type t = D.t
  >   let make x = D.from_int x
  > end
  > module A = F (struct type t = int let from_int x = x)
  > module B = F (struct type t = int let from_int x = x)
  > open A
  > let f x = B.make (A.make x)
  > EOF
  $ $MERLIN single type-enclosing -position 11:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  int -> B.t

  $ echo "" > .merlin
  > cat > sample.ml <<EOF
  > module F (D: sig type t val from_int : int -> t end) : sig
  >   type t
  >   val make : int -> t
  > end = struct
  >   type t = D.t
  >   let make x = D.from_int x
  > end
  > module A = F (struct type t = int let from_int x = x)
  > module B = F (struct type t = int let from_int x = x)
  > open B
  > let f x = B.make (A.make x)
  > EOF
  $ $MERLIN single type-enclosing -position 11:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  int -> B.t

  $ echo "FLG -short-paths" > .merlin
  > cat > sample.ml <<EOF
  > module F (D: sig type t val from_int : int -> t end) : sig
  >   type t
  >   val make : int -> t
  > end = struct
  >   type t = D.t
  >   let make x = D.from_int x
  > end
  > module A = F (struct type t = int let from_int x = x)
  > module B = F (struct type t = int let from_int x = x)
  > open B
  > let f x = B.make (A.make x)
  > EOF
  $ $MERLIN single type-enclosing -position 11:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  int -> t
