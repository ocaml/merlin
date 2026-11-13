Record-filed completion should preserve deprecation attributes.

  $ cat > record.ml <<'EOF'
  > type t = { legacy : int [@deprecated] [@ocaml.doc "Legacy field"]; current : int }
  > let get (r : t) = r.legacy
  > let make : t = {legacy = 1; current = 2 } 
  > let get_current (r : t) =  r.current
  > let make_current : t = {current = 2; legacy = 1}
  > EOF

Complete a deprecated field in a field access.

  $ $MERLIN single complete-prefix \
  > -position 2:23 \
  > -prefix r.leg \
  > -filename record.ml < record.ml \
  > | jq -c '.value.entries | map({name, deprecated})'
  [{"name":"legacy","deprecated":true}]

Complete a deprecated field in a record construction.

  $ $MERLIN single complete-prefix \
  > -position 3:20 \
  > -prefix leg \
  > -filename record.ml < record.ml \
  > | jq -c '.value.entries | map({name, deprecated})'
  [{"name":"legacy","deprecated":true}]

Non-deprecated record fields should not be marked as deprecated.

  $ $MERLIN single complete-prefix \
  >   -position 4:31 -prefix r.cur \
  >   -filename record.ml < record.ml \
  >   | jq -c '.value.entries | map({name, deprecated})'
  [{"name":"current","deprecated":false}]

  $ $MERLIN single complete-prefix \
  >   -position 5:28 -prefix cur \
  >   -filename record.ml < record.ml \
  >   | jq -c '.value.entries | map({name, deprecated})'
  [{"name":"current","deprecated":false}]


Record-field completion should preserve documentation attributes.

  $ $MERLIN single complete-prefix \
  >   -position 2:23 -prefix r.leg \
  >   -filename record.ml < record.ml \
  >   | jq -c '.value.entries | map({name, info})'
  [{"name":"legacy","info":"Legacy field"}]

Documentation should also be preserved in record construction.

  $ $MERLIN single complete-prefix \
  >   -position 3:20 -prefix leg \
  >   -filename record.ml < record.ml \
  >   | jq -c '.value.entries | map({name, info})'
  [{"name":"legacy","info":"Legacy field"}]
