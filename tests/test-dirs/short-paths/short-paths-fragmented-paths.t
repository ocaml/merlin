Some variation on partial opening

  $ echo "" > .merlin
  > cat > sample.ml <<EOF
  > module A = struct
  >   module B = struct
  >     type t = string
  >     module C = struct
  >        type nonrec t =
  >         | Foo of string
  >         | Bar of t
  >     end
  >     type c = C.t
  >   end
  >   type r = B.c
  > end
  > open A
  > let f x y = (A.B.C.Foo x, A.B.C.Bar y)
  > EOF
  $ $MERLIN single type-enclosing -position 14:5 -filename sample.ml < sample.ml \
  > | jq .value[0].type -r
  string -> A.B.t -> A.B.C.t * A.B.C.t


  $ echo "FLG -short-paths" > .merlin
  > cat > sample.ml <<EOF
  > module A = struct
  >   module B = struct
  >     type t = string
  >     module C = struct
  >        type nonrec t =
  >         | Foo of string
  >         | Bar of t
  >     end
  >     type c = C.t
  >   end
  >   type r = B.c
  > end
  > open A
  > let f x y = (A.B.C.Foo x, A.B.C.Bar y)
  > EOF
  $ $MERLIN single type-enclosing -position 14:5 -filename sample.ml < sample.ml \
  > | jq .value[0].type -r
  string -> string -> r * r

  $ echo "FLG -short-paths" > .merlin
  > cat > sample.ml <<EOF
  > module A = struct
  >   module B = struct
  >     type t = string
  >     module C = struct
  >        type nonrec t =
  >         | Foo of string
  >         | Bar of t
  >     end
  >     type c = C.t
  >   end
  >   type r = B.c
  > end
  > open A.B
  > let f x y = (A.B.C.Foo x, A.B.C.Bar y)
  > EOF
  $ $MERLIN single type-enclosing -position 14:5 -filename sample.ml < sample.ml \
  > | jq .value[0].type -r
  t -> t -> c * c

  $ echo "FLG -short-paths" > .merlin
  > cat > sample.ml <<EOF
  > module A = struct
  >   module B = struct
  >     type t = string
  >     module C = struct
  >        type nonrec t =
  >         | Foo of string
  >         | Bar of t
  >     end
  >     type c = C.t
  >   end
  >   type r = B.c
  > end
  > open A.B.C
  > let f x y = (A.B.C.Foo x, A.B.C.Bar y)
  > EOF
  $ $MERLIN single type-enclosing -position 14:5 -filename sample.ml < sample.ml \
  > | jq .value[0].type -r
  string -> string -> t * t

  $ echo "FLG -short-paths" > .merlin
  > cat > sample.ml <<EOF
  > module A = struct
  >   module B = struct
  >     type t = string
  >     module C = struct
  >        type nonrec t =
  >         | Foo of string
  >         | Bar of t
  >     end
  >     type c = C.t
  >   end
  >   type r = B.c
  > end
  > open A
  > open B
  > open C
  > let f x y = (A.B.C.Foo x, A.B.C.Bar y)
  > EOF
  $ $MERLIN single type-enclosing -position 16:5 -filename sample.ml < sample.ml \
  > | jq .value[0].type -r
  t -> t -> c * c
