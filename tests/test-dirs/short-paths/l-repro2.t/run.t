
Priv
  $ $OCAMLC -c -short-paths -no-alias-deps -I priv -o priv/topic.cmi -c -intf priv/topic.mli 
  $ $OCAMLC -c -short-paths -no-alias-deps -I priv -o priv/topic_name.cmi -c -intf priv/topic_name.mli 
  $ $OCAMLC -c -short-paths -no-alias-deps -I priv -o priv/repro2_priv.cmo -c -impl priv/repro2_priv.ml


  $ ls priv/*.cmi
  priv/repro2_priv.cmi
  priv/topic.cmi
  priv/topic_name.cmi

Types
  $ $OCAMLC -c -short-paths -no-alias-deps -o types/repro2_types__.cmo -c -impl types/repro2_types__.ml-gen 2>/dev/null
  $ $OCAMLC -c -short-paths -no-alias-deps -I priv -I types -open Repro2_types__ -o types/repro2_types.cmo -c -impl types/repro2_types.ml
  $ $OCAMLC -c -short-paths -no-alias-deps -I priv -I types -open Repro2_types__ -o types/repro2_types__Topic.cmi -c -intf types/topic.mli 
  $ $OCAMLC -c -short-paths -no-alias-deps -I priv -I types -open Repro2_types__ -o types/repro2_types__Topic_name.cmi -c -intf types/topic_name.mli 

  $ ls types/*.cmi
  types/repro2_types.cmi
  types/repro2_types__.cmi
  types/repro2_types__Topic.cmi
  types/repro2_types__Topic_name.cmi

  $ ocamlobjinfo -quiet -discourse types/repro2_types.cmi
  Discourse:
  Topic: alias: Topic [Repro2_types__!.Topic] Repro2_types__!.Topic
  
  Topic_name: alias: Topic_name [Repro2_types__!.Topic_name]
    Repro2_types__!.Topic_name
  


  $ ocamlobjinfo -quiet -discourse types/repro2_types__Topic_name.cmi
  Discourse:
  t: Repro2_priv!.Topic_name.t
  of_topic: option/12!; t/286; Repro2_types__!.Topic.t


Main
  $ $OCAMLC -c -short-paths -no-alias-deps -H priv -I types -o main/repro2_main.cmo -c -impl main/repro2_main.ml

  $ ls main/*.cmi
  main/repro2_main.cmi

  $ ocamlobjinfo -quiet -discourse main/repro2_main.cmi
  Discourse:
  Topic: alias: Topic [Repro2_types!.Topic] Repro2_types!.Topic
  
  Topic_name: alias: Topic_name [Repro2_types!.Topic_name]
    Repro2_types!.Topic_name
  


Usage
  $ $OCAMLC -c -short-paths -no-alias-deps -H priv -I types -I main -I usage usage/usage.cmo -c -impl usage/usage.ml

  $ ls usage/*.cmi
  usage/usage.cmi


  $ cat >.merlin <<'EOF'
  > FLG -short-paths -nostdlib
  > B .
  > BH priv
  > B types
  > B main
  > B usage
  > EOF

We expect Repro2_main.Topic.t
  $ $MERLIN single type-enclosing -position 1:40 -index 0 \
  > -filename usage/usage.ml <usage/usage.ml | jq '.value[0].type'
  "Repro2_main.Topic.t -> Repro2_main.Topic_name.t option"
