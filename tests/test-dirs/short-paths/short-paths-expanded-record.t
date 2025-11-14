Mix and match of alias and primitive types

  $ echo "" > .merlin
  > cat > sample.ml <<EOF
  > type s = string
  > type i = int
  > type t = {
  >   a: int
  > ; b: i * int * s * string
  > ; c: (i, string) result
  > }
  > let f a b c = {a; b; c}
  > EOF
  $ $MERLIN single type-enclosing -position 8:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  int -> i * int * s * string -> (i, string) result -> t

  $ echo "FLG -short-paths" > .merlin
  > cat > sample.ml <<EOF
  > type s = string
  > type i = int
  > type t = {
  >   a: int
  > ; b: i * int * s * string
  > ; c: (i, string) result
  > }
  > let f a b c = {a; b; c}
  > EOF
  $ $MERLIN single type-enclosing -position 8:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  i -> i * i * s * s -> (i, s) result -> t

Mix and match of alias and primitive types with opening

  $ echo "" > .merlin
  > cat > sample.ml <<EOF
  > type s = string
  > type i = int
  > type t = {
  >   a: int
  > ; b: i * int * s * string
  > ; c: (i, string) result
  > }
  > module M = struct
  >   type new_string = s
  >   type new_int = i
  > end
  > open M
  > let f a b c = {a; b; c}
  > EOF
  $ $MERLIN single type-enclosing -position 13:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  int -> i * int * s * string -> (i, string) result -> t

  $ echo "FLG -short-paths" > .merlin
  > cat > sample.ml <<EOF
  > type s = string
  > type i = int
  > type t = {
  >   a: int
  > ; b: i * int * s * string
  > ; c: (i, string) result
  > }
  > module M = struct
  >   type new_string = s
  >   type new_int = i
  > end
  > open M
  > let f a b c = {a; b; c}
  > EOF
  $ $MERLIN single type-enclosing -position 13:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type
  "new_int -> new_int * new_int * new_string * new_string -> (new_int, new_string) result -> t"
