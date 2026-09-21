Reproduce a short-paths bug where a wrapped library's mangled name leaks
into the printed type. With dune-style wrapping, querying the type of
`Repro2_main.Topic_name.of_topic` in `usage/src/usage.ml` should print
`Repro2_main.Topic.t -> Repro2_main.Topic_name.t option`, but instead
prints `Repro2_types__.Topic.t -> Repro2_main.Topic_name.t option`.

Build the priv library (no dependencies).
  $ cat > priv/src/repro2_priv__.ml-gen << 'EOF'
  > module No_direct_access_to_repro2_priv = struct
  >   module Repro2_priv = No_such_module
  >   module Repro2_priv__Topic = No_such_module
  >   module Repro2_priv__Topic_name = No_such_module
  > end
  > 
  > (** @canonical Repro2_priv.Repro2_priv *)
  > module Repro2_priv = Repro2_priv
  > 
  > (** @canonical Repro2_priv.Topic *)
  > module Topic = Repro2_priv__Topic
  > 
  > (** @canonical Repro2_priv.Topic_name *)
  > module Topic_name = Repro2_priv__Topic_name
  > EOF

  $ $OCAMLC -w -49 -no-alias-deps -o priv/src/repro2_priv__.cmo \
  >   -c -impl priv/src/repro2_priv__.ml-gen
  $ $OCAMLC -I priv/src -open Repro2_priv__ \
  >   -o priv/src/repro2_priv__Topic.cmi -c -intf priv/src/topic.mli
  $ $OCAMLC -I priv/src -open Repro2_priv__ \
  >   -o priv/src/repro2_priv__Topic.cmo -c -impl priv/src/topic.ml
  $ $OCAMLC -I priv/src -open Repro2_priv__ \
  >   -o priv/src/repro2_priv__Topic_name.cmi -c -intf priv/src/topic_name.mli
  $ $OCAMLC -I priv/src -open Repro2_priv__ \
  >   -o priv/src/repro2_priv__Topic_name.cmo -c -impl priv/src/topic_name.ml
  $ $OCAMLC -I priv/src -open Repro2_priv__ \
  >   -o priv/src/repro2_priv.cmo -c -impl priv/src/repro2_priv.ml

Build the types library (depends on priv).
  $ cat > types/src/repro2_types__.ml-gen << 'EOF'
  > module No_direct_access_to_repro2_types = struct
  >   module Repro2_types = No_such_module
  >   module Repro2_types__Topic = No_such_module
  >   module Repro2_types__Topic_name = No_such_module
  > end
  > 
  > (** @canonical Repro2_types.Repro2_types *)
  > module Repro2_types = Repro2_types
  > 
  > (** @canonical Repro2_types.Topic *)
  > module Topic = Repro2_types__Topic
  > 
  > (** @canonical Repro2_types.Topic_name *)
  > module Topic_name = Repro2_types__Topic_name
  > EOF

  $ $OCAMLC -w -49 -no-alias-deps -o types/src/repro2_types__.cmo \
  >   -c -impl types/src/repro2_types__.ml-gen
  $ $OCAMLC -I types/src -I priv/src -open Repro2_types__ \
  >   -o types/src/repro2_types__Topic.cmi -c -intf types/src/topic.mli
  $ $OCAMLC -I types/src -I priv/src -open Repro2_types__ \
  >   -o types/src/repro2_types__Topic.cmo -c -impl types/src/topic.ml
  $ $OCAMLC -I types/src -I priv/src -open Repro2_types__ \
  >   -o types/src/repro2_types__Topic_name.cmi -c -intf types/src/topic_name.mli
  $ $OCAMLC -I types/src -I priv/src -open Repro2_types__ \
  >   -o types/src/repro2_types__Topic_name.cmo -c -impl types/src/topic_name.ml
  $ $OCAMLC -I types/src -I priv/src -open Repro2_types__ \
  >   -o types/src/repro2_types.cmo -c -impl types/src/repro2_types.ml

Build the main library (depends on types).
  $ cat > main/src/repro2_main__.ml-gen << 'EOF'
  > module No_direct_access_to_repro2_main = struct
  >   module Repro2_main = No_such_module
  > end
  > 
  > (** @canonical Repro2_main.Repro2_main *)
  > module Repro2_main = Repro2_main
  > EOF

  $ $OCAMLC -w -49 -no-alias-deps -o main/src/repro2_main__.cmo \
  >   -c -impl main/src/repro2_main__.ml-gen
  $ $OCAMLC -I main/src -I types/src -I priv/src -open Repro2_main__ \
  >   -o main/src/repro2_main.cmo -c -impl main/src/repro2_main.ml

Create a .merlin in usage/src mirroring what dune would generate.
  $ cat > usage/src/.merlin << 'EOF'
  > FLG -short-paths
  > FLG -log-file log -log-section discourse-recap -nostdlib
  > FLG -open Repro2_standalone__
  > S .
  > B .
  > SH ../../priv/src
  > BH ../../priv/src
  > S ../../types/src
  > B ../../types/src
  > S ../../main/src
  > B ../../main/src
  > EOF

TODO suspicions duplications in u_paths ?

Expected `Repro2_main.Topic.t -> Repro2_main.Topic_name.t option`.
  $ $MERLIN single type-enclosing -position 1:40 -index 0 \
  > -filename usage/src/usage.ml < usage/src/usage.ml \
  > | jq '.value[0].type'
  "Repro2_main.Topic.t -> Repro2_main.Topic_name.t option"


$ cat log | sed -E 's/^# [0-9]+.[0-9]+/#/'
