Simple alias on string

  $ echo "" > .merlin
  > cat > sample.ml <<EOF
  > type t = string
  > let f = String.lowercase_ascii
  > EOF
  $ $MERLIN single type-enclosing -position 2:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  string -> string

  $ echo "FLG -short-paths" > .merlin
  > cat > sample.ml <<EOF
  > type t = string
  > let f = String.lowercase_ascii
  > EOF
  $ $MERLIN single type-enclosing -position 2:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  t -> t

Simple alias on string and ascription

  $ echo "" > .merlin
  > cat > sample.ml <<EOF
  > type t = string
  > let f : t -> string = String.lowercase_ascii
  > EOF
  $ $MERLIN single type-enclosing -position 2:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  t -> string

  $ echo "FLG -short-paths" > .merlin
  > cat > sample.ml <<EOF
  > type t = string
  > let f : t -> string = String.lowercase_ascii
  > EOF
  $ $MERLIN single type-enclosing -position 2:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  t -> t

Simple alias on string and int

  $ echo "" > .merlin
  > cat > sample.ml <<EOF
  > type s_string = string
  > type s_int = int
  > let f = String.length
  > EOF
  $ $MERLIN single type-enclosing -position 3:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  string -> int

  $ echo "FLG -short-paths" > .merlin
  > cat > sample.ml <<EOF
  > type s_string = string
  > type s_int = int
  > let f = String.length
  > EOF
  $ $MERLIN single type-enclosing -position 3:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  s_string -> s_int

Simple alias on string and int, with ascription

  $ echo "" > .merlin
  > cat > sample.ml <<EOF
  > type s_string = string
  > type s_int = int
  > let f : string -> s_int = String.length
  > EOF
  $ $MERLIN single type-enclosing -position 3:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  string -> s_int

  $ echo "FLG -short-paths" > .merlin
  > cat > sample.ml <<EOF
  > type s_string = string
  > type s_int = int
  > let f : string -> s_int = String.length
  > EOF
  $ $MERLIN single type-enclosing -position 3:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  s_string -> s_int
