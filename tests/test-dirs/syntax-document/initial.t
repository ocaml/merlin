  $ cat >doc.ml <<EOF
  > let square x = x * x
  > (** Calculates the square of a number *)
  > EOF

  $ $MERLIN single syntax-document -position 1:5 \
  > -filename doc.ml <doc.ml | jq '.value'
  "No documentation available"

