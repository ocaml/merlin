We get a bad path for `hello`

  $ cat > foo.ml <<EOF
  > module Foo = struct
  >   type t
  > end
  > 
  > module Bar = struct
  >   module Foo = struct
  >     type t = Foo.t
  >   end
  > end
  > 
  > open! Bar
  > 
  > let hello : Foo.t = 0
  > EOF

It happens regardless of whether short-paths is enabled
  $ $MERLIN single type-enclosing -position 13:5 -filename foo.ml < foo.ml | jq .value[0].type -r
  Bar.Foo.t

  $ echo "FLG -short-paths" > .merlin
  $ $MERLIN single type-enclosing -position 13:5 -filename foo.ml < foo.ml | jq .value[0].type -r
  Bar.Foo.t

It seems to be related to shadowing somehow. This works:
  $ cat > foo.ml <<EOF
  > module Foo = struct
  >   type t
  > end
  > 
  > module Bar = struct
  >   module Baz = struct
  >     type t = Foo.t
  >   end
  > end
  > 
  > open! Bar
  > 
  > let hello : Baz.t = 0
  > EOF

  $ $MERLIN single type-enclosing -position 13:5 -filename foo.ml < foo.ml | jq .value[0].type -r
  Foo.t
