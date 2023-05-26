  $ cat >main.ml <<EOF
  > module type Empty = sig end
  > 
  > module Foo = struct
  >   module type S = Empty
  > 
  >   let x = ()
  > end
  > 
  > module type Foo = Foo.S
  > 
  > let f (module M : Foo) = ()
  > 
  > module S : sig
  >   val f : (module Foo) -> unit
  > end = struct
  >   let f (module M : Foo) = ()
  > end
  > EOF

  $ $MERLIN single errors -filename main.ml <main.ml
  {
    "class": "return",
    "value": [],
    "notifications": []
  }

That Foo is the module type Foo.S, not the module Foo
  $ $MERLIN single type-enclosing -position 11:21 \
  > -filename main.ml <main.ml | jq '.value[0]'
  {
    "start": {
      "line": 11,
      "col": 18
    },
    "end": {
      "line": 11,
      "col": 21
    },
    "type": "Foo.S",
    "tail": "no"
  }


That Foo is the module type Foo.S, not the module Foo
  $ $MERLIN single type-enclosing -position 14:19 \
  > -filename main.ml <main.ml | jq '.value[0]'
  {
    "start": {
      "line": 14,
      "col": 18
    },
    "end": {
      "line": 14,
      "col": 21
    },
    "type": "Foo.S",
    "tail": "no"
  }
