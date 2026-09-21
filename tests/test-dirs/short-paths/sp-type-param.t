  $ cat >test.ml <<'EOF'
  > module V = struct 
  >   module type S = sig type 'a t val return : 'a -> 'a t end
  > end
  > module type Monad = sig
  >   type 'a t
  >   include V.S
  >     with type 'a t := 'a t
  > EOF

  $ echo "FLG -short-paths" > .merlin

  $ $MERLIN single type-enclosing -position 4:21 \
  > -filename test.ml < test.ml | jq '.value[0].type'
  "sig type 'a t val return : 'a -> 'a t end"
