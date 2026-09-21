  $ cat > foo.ml <<EOF
  > module A = struct
  >   type r = int
  >   type s = string
  >   type t = unit
  > end;;
  > 
  > (* parens *)
  > type x = A.(r * s * t);;
  > let y : x = assert false
  > EOF


  $ echo "FLG -short-paths" > .merlin

At some point this was raising because of wrong environments
  $ $MERLIN single type-enclosing -position 9:4 \
  > -filename foo.ml < foo.ml | jq '.value[].type'
  "x"

