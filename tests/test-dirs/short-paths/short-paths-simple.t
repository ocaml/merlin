Simple alias on string

  $ cat > sample.ml <<EOF
  > type t = string
  > let f = String.lowercase_ascii
  > EOF
  $ $MERLIN single type-enclosing -position 2:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  string -> string

  $ $MERLIN single type-enclosing -short-paths -position 2:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  t -> t

Simple alias on string and ascription

  $ cat > sample.ml <<EOF
  > type t = string
  > let f : t -> string = String.lowercase_ascii
  > EOF
  $ $MERLIN single type-enclosing -position 2:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  t -> string

  $ $MERLIN single type-enclosing -short-paths -position 2:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  t -> t

Simple alias on string and int

  $ cat > sample.ml <<EOF
  > type s_string = string
  > type s_int = int
  > let f = String.length
  > EOF
  $ $MERLIN single type-enclosing -position 3:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  string -> int

  $ $MERLIN single type-enclosing -short-paths -position 3:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  s_string -> s_int

Simple alias on string and int, with ascription

  $ cat > sample.ml <<EOF
  > type s_string = string
  > type s_int = int
  > let f : string -> s_int = String.length
  > EOF
  $ $MERLIN single type-enclosing -position 3:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  string -> s_int

  $ $MERLIN single type-enclosing -short-paths -position 3:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  s_string -> s_int
