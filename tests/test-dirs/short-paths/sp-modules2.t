  $ cat > bar.mli <<EOF
  > module A : sig
  >   type t
  >   module B : sig
  >     module C : sig
  >       type s
  >       module D : sig
  >         val foo : t -> s
  >       end
  >     end
  >   end
  > end

  $ cat >bar.mli <<EOF
  > module A : sig
  >   type t
  >   module B : sig
  >     module C : sig
  >       type s
  >     end
  >     module D : sig
  >       val foo : t -> C.s
  >     end
  >   end
  > end
  > EOF

  $ $OCAMLC -c bar.mli -o bar.cmi

  $ cat > foo.ml <<EOF
  > open Bar
  > open A.B
  > let f x = D.foo x
  > EOF

> -log-file - -log-section discourse,type-enclosing,short-paths 
  $ $MERLIN single type-enclosing -short-paths -position 3:13 \
  > -filename foo.ml < foo.ml | jq '.value[0]'
  {
    "start": {
      "line": 3,
      "col": 10
    },
    "end": {
      "line": 3,
      "col": 15
    },
    "type": "A.t -> C.s",
    "tail": "no"
  }

$ $MERLIN single type-enclosing -short-paths -log-file - -log-section discourse-verbose -position 3:16 \
> -filename foo.ml < foo.ml | jq '.value[0]'
