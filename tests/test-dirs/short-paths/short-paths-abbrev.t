Alias on  virtual classes

  $ cat > sample.ml <<EOF
  > class type a = object
  >   method foo : int -> int
  > end
  > type t = a
  > let f (a: a) x = a#foo x
  > EOF
  $ $MERLIN single type-enclosing -position 5:5 -filename sample.ml < sample.ml \
  > | jq .value[0].type -r
  a -> int -> int

  $ $MERLIN single type-enclosing -short-paths -position 5:5 -filename sample.ml < sample.ml \
  > | jq .value[0].type -r
  t -> int -> int

Alias on concrete classes

  $ cat > sample.ml <<EOF
  > class a ~foo = object
  >   val bar = foo + 1
  >   method f () = bar
  > end
  > type t = a
  > let f foo = new a ~foo
  > EOF
  $ $MERLIN single type-enclosing -position 6:5 -filename sample.ml < sample.ml \
  > | jq .value[0].type -r
  int -> a

  $ $MERLIN single type-enclosing -short-paths -position 6:5 -filename sample.ml < sample.ml \
  > | jq .value[0].type -r
  int -> t
