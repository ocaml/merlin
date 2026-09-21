  $ echo "FLG -short-paths" > .merlin
  $ cat >test.ml <<'EOF'
  > module Long = struct module Path = struct type t = T end end 
  > module M = struct 
  >   type t = Long.Path.t
  >   let x : t = A
  > end;;
  > ignore M.x
  > EOF


FIXME: sig type t = Long.Path.t val x : t end
  $ $MERLIN single type-enclosing -position 6:7 \
  > -filename test.ml < test.ml | jq '.value[0].type'
  "sig type t = M.t val x : t end"
