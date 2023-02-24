  $ cat >doc.ml <<EOF
  > (** whatever *)
  > let id () = ()
  > let _ = id ()
  > EOF

  $ $MERLIN single document -position 2:5 \
  > -filename doc.ml <doc.ml | jq '.value'
  "whatever"

  $ $MERLIN single document -position 3:9 \
  > -filename doc.ml <doc.ml | jq '.value'
  "whatever"
