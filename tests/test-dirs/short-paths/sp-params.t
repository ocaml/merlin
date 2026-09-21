
  $ echo "FLG -short-paths" > .merlin
  $ cat >test.ml <<'EOF'
  > module M = struct module N = struct type t = A of int end end 
  > module P = struct type 'a t = 'a end 
  > module X = M.N 
  > let x : M.N.t P.t = A 42
  > EOF

The shortest path is `X.t`
  $ $MERLIN single type-enclosing -position 4:4 \
  > -filename test.ml < test.ml | jq '.value[].type'
  "X.t"
