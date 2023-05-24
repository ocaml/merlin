  $ mkdir liba
  $ cat >liba/liba.ml <<EOF
  > module Libb = Libb
  > EOF

  $ mkdir libb
  $ cat >libb/libb.ml <<EOF
  > let x = 42
  > EOF

  $ cat >liba/.merlin <<EOF
  > B .
  > B ../libb
  > EOF

  $ (cd libb && $OCAMLC -c -bin-annot libb.ml)
  $ (cd liba && $OCAMLC -c -bin-annot liba.ml -I ../libb)


  $ ls libb
  libb.cmi
  libb.cmo
  libb.cmt
  libb.ml

There should be no errors
  $ cd liba && $MERLIN single errors -filename liba.ml<liba.ml |
  > jq '.value'
  []
