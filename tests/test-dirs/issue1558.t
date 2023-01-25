  $ cat >main.ml <<EOF
  > open Stdlib.Effect
  > EOF

The unstable alert should be disabled by default in OCaml 5
  $ $MERLIN single errors -filename main.ml <main.ml |
  > tr '\n' ' ' | jq '.value[0].message'
  null

But can be enabled
  $ $MERLIN single errors -filename main.ml -alert +unstable <main.ml |
  > tr '\n' ' ' | jq '.value[0].message'
  "Alert unstable: module Stdlib.Effect The Effect interface may change in incompatible ways in the future."
