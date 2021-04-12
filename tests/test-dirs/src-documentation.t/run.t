documentation for the last defined value (in the same file) is shown
  $ $MERLIN single document -position 7:10 -filename doc.ml < doc.ml | \
  > jq '.value'
  "second function"

documentation for the non-last defined value (in the same file) is show
(we care about "non-last" value because of issue #1261)
  $ $MERLIN single document -position 7:13 -filename doc.ml < doc.ml | \
  > jq '.value'
  "first function"

  $ $MERLIN single document -position 9:6 -filename doc.ml < doc.ml | \
  > jq '.value'
  "No documentation available"

  $ $MERLIN single document -position 9:22 -filename doc.ml < doc.ml | \
  > jq '.value'
  " List reversal. "

  $ dune build --root=. ./doc.exe

  $ $MERLIN single document -position 11:12 -filename doc.ml < doc.ml | \
  > jq '.value'
  " A function "
