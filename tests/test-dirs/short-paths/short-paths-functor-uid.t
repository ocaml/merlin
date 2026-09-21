Two instantiations of the same functor share the uid of the declaration in its
body, but they are not the same type: [A.u] is [int list] and [B.u] is
[string list]. The uid-keyed index in shorter_paths must not merge them, even
though normalization stops on both (their parameters do not match the arguments
of their manifest).

  $ cat >main.ml <<'EOF'
  > module F (X : sig type t end) = struct
  >   type u = X.t list
  > end
  > 
  > module A = F (struct type t = int end)
  > module B = F (struct type t = string end)
  > 
  > open A
  > 
  > let f (x : B.u) = x
  > EOF

We expect B.u -> B.u, and in particular not u -> u, which would name A.u
  $ $MERLIN single type-enclosing -short-paths -position 10:5 \
  > -filename main.ml < main.ml | jq '.value[0].type'
  "B.u -> B.u"
