Two modules with the same module type share the uids of its abstract types, but
they are not the same types.

  $ cat >mymap.ml <<'EOF'
  > module type S = sig type key type 'a t end
  > module Make (X : sig type t end) : S with type key = X.t = struct
  >   type key = X.t
  >   type 'a t = (key * 'a) list
  > end
  > EOF

  $ cat >name.mli <<'EOF'
  > type t = string
  > module Map : Mymap.S with type key = t
  > EOF

  $ cat >name.ml <<'EOF'
  > type t = string
  > module Map = Mymap.Make (String)
  > EOF

  $ cat >main.ml <<'EOF'
  > module Map = Mymap.Make (Int)
  > 
  > let f (x : int Name.Map.t) = x
  > 
  > let s = Name.(String.length "")
  > EOF

  $ $OCAMLC -c mymap.ml name.mli name.ml main.ml

  $ cat >.merlin <<'EOF'
  > FLG -short-paths
  > B .
  > S .
  > EOF

We expect [Name.Map.t], and in particular not [Map.t] which is another type
  $ $MERLIN single type-enclosing -position 3:5 \
  > -filename main.ml < main.ml | jq '.value[0].type'
  "int Name.Map.t -> int Name.Map.t"
