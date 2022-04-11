  $ dune exec ./main.exe
  42

  $ $MERLIN single locate -look-for ml -position 6:24 \
  > -filename ./main.ml < ./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/lib/dep_of_packed_mod.ml",
    "pos": {
      "line": 1,
      "col": 4
    }
  }
