  $ cat >main.ml <<EOF
  > module B = struct
  >   module type T = sig end
  > 
  >   module T = struct end
  > end
  > 
  > module M : B.T = struct end
  > EOF

FIXME: we expect line 2 here
  $ $MERLIN single locate -look-for mli -position 7:13 \
  > -filename ./main.ml < ./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/main.ml",
    "pos": {
      "line": 4,
      "col": 2
    }
  }

  $ $MERLIN single locate -look-for ml -position 7:13 \
  > -filename ./main.ml < ./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/main.ml",
    "pos": {
      "line": 2,
      "col": 2
    }
  }
