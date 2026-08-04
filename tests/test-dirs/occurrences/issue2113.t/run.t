Test for issue 2113: duplicate occurrence on recovered let binding

  $ $MERLIN single occurrences -identifier-at 1:9 -filename ./main.ml <<EOF
  > let rec ma
  > EOF

