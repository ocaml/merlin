The reader cache must distinguish implementations from interfaces, even when
their contents are identical. This test records the current incorrect reuse in
both directions.

  $ $MERLIN server stop-server

  $ cat > a.ml <<'EOF'
  > let x = 0
  > EOF
  $ cp a.ml b.mli

Without the cache, the implementation is valid and the interface has a syntax
error.

  $ touch .merlin
  $ $MERLIN server errors -filename a.ml < a.ml | jq '.value | map({type, message})'
  []
  $ $MERLIN server errors -filename b.mli < b.mli | jq '.value | map({type, message})'
  [
    {
      "type": "parser",
      "message": "Syntax error"
    }
  ]

Enable the cache and start with a fresh server, reading the implementation first.

  $ $MERLIN server stop-server
  $ cat > .merlin <<'EOF'
  > USE_PPX_CACHE
  > EOF
  $ $MERLIN server errors -filename a.ml < a.ml | jq '.value | map({type, message})'
  []

Switching to the interface does not reuse the implementation's parse result.

  $ $MERLIN server errors -filename b.mli -log-file merlin_logs < b.mli | jq '.value | map({type, message})'
  [
    {
      "type": "parser",
      "message": "Syntax error"
    }
  ]
  $ grep 'Phase cache' -A 1 merlin_logs | sed 's/[0-9]*//g'
  # . Phase cache - Reader phase
  Cache invalidation
  --
  # . Phase cache - PPX phase
  Cache invalidation

Start with a fresh server again, this time reading the interface first. The
syntax error is reported correctly on a cold cache.

  $ $MERLIN server stop-server
  $ $MERLIN server errors -filename b.mli < b.mli | jq '.value | map({type, message})'
  [
    {
      "type": "parser",
      "message": "Syntax error"
    }
  ]

Switching to the valid implementation does not reuse the interface's parse result.

  $ $MERLIN server errors -filename a.ml -log-file merlin_logs < a.ml | jq '.value | map({type, message})'
  []
  $ grep 'Phase cache' -A 1 merlin_logs | sed 's/[0-9]*//g'
  # . Phase cache - Reader phase
  Cache invalidation
  --
  # . Phase cache - PPX phase
  Cache invalidation

  $ $MERLIN server stop-server
