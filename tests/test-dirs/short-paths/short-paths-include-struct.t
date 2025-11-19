# Type exposed by include struct without ascription

  $ cat > sample.ml <<EOF
  > include struct
  >   type 'a t = 'a option
  > end
  > let f x = Some x
  > EOF
  $ $MERLIN single type-enclosing -position 4:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  'a -> 'a option

  $ $MERLIN single type-enclosing -short-paths -position 4:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  'a -> 'a t

# Type exposed by include struct with ascription

  $  cat > sample.ml <<EOF
  > include struct
  >   type 'a t = 'a option
  > end
  > let f x : int -> int option = Some x
  > EOF
  $ $MERLIN single type-enclosing -position 4:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  'a -> int -> int option

  $ $MERLIN single type-enclosing -short-paths -position 4:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  'a -> int -> int t
