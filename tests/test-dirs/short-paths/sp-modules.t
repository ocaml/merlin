  $ cat > foo.ml <<EOF
  > module Empty = struct end
  > module type S = sig type t module M : sig end end
  > module Bar : S with module M = Empty = struct
  >   type t = int
  >   module M = Empty
  > end
  > module Foo = Bar
  > type u = Foo.t
  > EOF

$ $MERLIN single dump -what parsetree -filename foo.ml < foo.ml

  $ $MERLIN single errors -filename foo.ml < foo.ml  
  {
    "class": "return",
    "value": [],
    "notifications": []
  }

  $ echo "FLG -short-paths" > .merlin

> -log-file - -log-section discourse,type-enclosing,short-paths 
  $ $MERLIN single type-enclosing -position 8:14 \
  > -filename foo.ml < foo.ml 
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 8,
          "col": 9
        },
        "end": {
          "line": 8,
          "col": 14
        },
        "type": "Foo.t",
        "tail": "no"
      },
      {
        "start": {
          "line": 8,
          "col": 9
        },
        "end": {
          "line": 8,
          "col": 14
        },
        "type": "Foo.t",
        "tail": "no"
      },
      {
        "start": {
          "line": 8,
          "col": 0
        },
        "end": {
          "line": 8,
          "col": 14
        },
        "type": "type u = Foo.t",
        "tail": "no"
      }
    ],
    "notifications": []
  }


  $ cat > substs.ml <<EOF
  > module A = struct module B = struct module C = struct type t end end end
  > module N = A.B
  > module M = N.C
  > let x : A.B.C.t = assert false
  > EOF

  $ $MERLIN single type-enclosing -position 4:4 \
  > -filename substs.ml < substs.ml | jq '.value[].type'
  "M.t"


  $ cat > substs.ml <<EOF
  > module W = struct
  >   module A = struct module B = struct module C = struct type t end end end
  >   module N = A.B
  >   module M = N.C
  > end
  > let x : W.A.B.C.t = assert false
  > open W 
  > let x : A.B.C.t = assert false
  > EOF

  $ $MERLIN single type-enclosing -position 6:4 \
  >  -filename substs.ml < substs.ml  | jq '.value[].type'
  "W.M.t"

  $ $MERLIN single type-enclosing -position 8:4 \
  > -filename substs.ml < substs.ml | jq '.value[].type'
  "M.t"

  $ cat >open.ml <<EOF
  > module A = struct type a = int end
  > include A 
  > let x = (5 : a)
  > EOF

  $ $MERLIN single type-enclosing -position 3:4 \
  > -filename open.ml <open.ml | jq '.value[].type'
  "int"

  $ cat >open.ml <<EOF
  > module A = struct type a = int end
  > open A 
  > let x = (5 : a)
  > EOF

  $ $MERLIN single type-enclosing -position 3:4 \
  > -filename open.ml <open.ml | jq '.value[].type'
  "int"

Open + Subst

  $ cat >open.ml <<EOF
  > module A = struct module B = struct module C = struct type a = V end end end
  > module X = A.B.C
  > open A.B.C
  > let x = (V : X.a)
  > EOF

Should be a and not X.a because of the open
  $ $MERLIN single type-enclosing -position 4:4 \
  > -filename open.ml <open.ml | jq '.value[].type'
  "a"

Test with a module aliases "loop":

  $ cat >loop.ml <<EOF
  > module N = struct type t = int end
  > module M = N
  > module X = struct 
  >   module M = struct type t = int end
  >   module N = M
  > end
  > let x : N.t = 5
  > EOF

  $ $MERLIN single type-enclosing -position 7:4 \
  > -filename loop.ml <loop.ml | jq '.value[0].type'
  "int"
