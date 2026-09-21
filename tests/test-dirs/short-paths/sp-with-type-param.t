  $ echo "FLG -short-paths" > .merlin

  $ cat >test.ml <<'EOF'
  > module Ident = struct
  >   type 'a t = 'a
  >   let return = Fun.id
  > end
  > EOF


  $ $MERLIN single type-enclosing -position 1:10 \
  > -filename test.ml < test.ml | jq '.value[0].type'
  "sig type 'a t = 'a val return : 'a -> 'a end"
