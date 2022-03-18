This test is for testing the behavior of identifiers with a . in them:

  $ cat >issue949.ml <<EOF
  > module A = struct let (+.) a b = a +. b end
  > let f x = A.(x +. 1.)
  > EOF


  $ $MERLIN single locate -look-for ml -position 2:16 ./issue949.ml <./issue949.ml
  {
    "class": "return",
    "value": {
      "file": "*buffer*",
      "pos": {
        "line": 1,
        "col": 22
      }
    },
    "notifications": []
  }
