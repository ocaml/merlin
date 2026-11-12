Record-filed completion should preserve deprecation attributes.

  $ cat > record.ml <<'EOF'
  > type t = { legacy : int [@deprecated]; current : int }
  > let get (r : t) = r.legacy
  > let make : t = {legacy = 1; current = 2 } 
  > EOF

Complete a deprecated field in a field access.

  $ $MERLIN single complete-prefix \
  > -position 2:23 \
  > -prefix r.leg \
  > -filename record.ml < record.ml \
  > | jq -c '.value.entries | map({name, deprecated})'
  [{"name":"legacy","deprecated":false}]

Complete a deprecated field in a record construction.

  $ $MERLIN single complete-prefix \
  > -position 3:20 \
  > -prefix leg \
  > -filename record.ml < record.ml \
  > | jq -c '.value.entries | map({name, deprecated})'
  [{"name":"legacy","deprecated":false}]
