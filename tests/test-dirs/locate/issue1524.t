  $ cat >main.ml <<EOF
  > type t = {
  >   data : int;
  > }
  > let f x =
  >   x.data + 1
  > EOF


  $ $MERLIN single locate -look-for mli -position 5:6 \
  > -filename ./main.ml < ./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/main.ml",
    "pos": {
      "line": 2,
      "col": 2
    }
  }

