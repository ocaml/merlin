  $ $MERLIN server stop-server

  $ cat >main.ml <<'EOF'
  > module X = struct type u = A end
  > 
  > module T = struct
  >   type 'a t = 'a option list
  > end
  > 
  > module M = struct
  >   include T
  >   let id (x : 'a T.t) = x
  > end
  > 
  > include M
  > 
  > let y = X.A
  > EOF

In single mode the signature of [M] is printed with [t]
  $ $MERLIN single type-enclosing -short-paths -position 12:9 \
  > -filename main.ml < main.ml | jq '.value[0].type'
  "sig type 'a t = 'a option list val id : 'a t -> 'a t end"

A first query, after [include M], fills the short-paths state
  $ $MERLIN server type-enclosing -short-paths -position 14:5 \
  > -filename main.ml < main.ml | jq '.value[0].type'
  "X.u"

We expect the same result as in single mode
  $ $MERLIN server type-enclosing -short-paths -position 12:9 \
  > -filename main.ml < main.ml | jq '.value[0].type'
  "sig type 'a t = 'a option list val id : 'a t -> 'a t end"

  $ $MERLIN server stop-server
