  $ cat >prefix.ml <<EOF
  > let ( ?? ) x = 2 * x in
  > print_int (?? 21)
  > EOF

  $ $OCAMLC -o p.exe prefix.ml
  $ ./p.exe
  42

Fixed: Old holes where interfering with operators (??). And Merlin would report
the folloswng error: "-      "message": "let-extension (with punning) expected."
  $ $MERLIN single errors \
  > -filename prefix.ml <prefix.ml |
  > jq '.value'
  []
