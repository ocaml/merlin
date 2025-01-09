Merlin can distinguish between two files with the same name and contents

  $ dune build @check

Part 1: Test that two files with the same name but different contents can be distinguished

Get Lib_a.Different.foo
  $ $MERLIN single locate -position 4:26 -filename bin/main.ml < bin/main.ml \
  >   | jq .value.file -r
  $TESTCASE_ROOT/lib_a/different.ml

Get Lib_b.Different.foo
  $ $MERLIN single locate -position 5:26 -filename bin/main.ml < bin/main.ml \
  >   | jq .value.file -r
  $TESTCASE_ROOT/lib_b/different.ml

Part 2: Test that two files with the same name and same contents can be distinguished

Get Lib_a.Same.foo
  $ $MERLIN single locate -position 1:22 -filename bin/main.ml < bin/main.ml \
  >   | jq .value.file -r
  $TESTCASE_ROOT/lib_a/same.ml

Get Lib_b.Same.foo
  $ $MERLIN single locate -position 2:22 -filename bin/main.ml < bin/main.ml \
  >   | jq .value.file -r
  $TESTCASE_ROOT/lib_b/same.ml
