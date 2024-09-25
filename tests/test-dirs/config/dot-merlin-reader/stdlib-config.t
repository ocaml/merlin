The STDLIB directive in .merlin is respected
  $ cat > .merlin <<EOF
  > STDLIB /stdlib1
  > EOF

  $ echo | $MERLIN single dump-configuration -filename test.ml 2> /dev/null | jq '.value.merlin.stdlib'
  "/stdlib1"

  $ rm .merlin

The -ocamlib-path flag is respected
  $ echo | $MERLIN single dump-configuration -ocamllib-path /stdlib2 -filename test.ml 2> /dev/null | jq '.value.merlin.stdlib'
  "/stdlib2"

The STDLIB directive in .merlin takes priority over -ocamllib-path
  $ cat > .merlin <<EOF
  > STDLIB /stdlib-from-.merlin
  > EOF

  $ echo | $MERLIN single dump-configuration -ocamllib-path /stdlib-from-flag -filename test.ml 2> /dev/null | jq '.value.merlin.stdlib'
  "/stdlib-from-.merlin"
