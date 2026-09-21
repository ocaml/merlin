  $ cat >shadow_stdlib.ml <<'EOF'
  > include Stdlib
  > EOF

  $ cat >import0.ml <<'EOF'
  > include (
  >   Shadow_stdlib :
  >     module type of struct
  >       include Shadow_stdlib
  >     end)
  > 
  > module Stdlib = struct
  >   include Stdlib
  > 
  >   module Domain = struct
  >     include Stdlib.Domain
  >   end
  > end

  $ $OCAMLC -c shadow_stdlib.ml import0.ml

Domain should not be prefixed
  $ $MERLIN single type-enclosing -short-paths -position 12:4 \
  > -filename import0.ml <import0.ml | tr '\n' ' ' | jq '.value[0].type' | head -c 32
  "sig   type 'a t = 'a Domain.t  
