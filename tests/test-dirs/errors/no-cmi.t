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

FIXME: the cmi do exist, there should be no errors
  $ cd liba && $MERLIN single errors -filename liba.ml<liba.ml |
  > jq '.value'
  [
    {
      "start": {
        "line": 1,
        "col": 14
      },
      "end": {
        "line": 1,
        "col": 18
      },
      "type": "warning",
      "sub": [],
      "valid": true,
      "message": "Warning 49: no cmi file was found in path for module Libb"
    }
  ]
