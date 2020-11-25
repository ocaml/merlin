A simple name is not expanded

  $ echo | $MERLIN single dump-configuration -filename relative_path.ml -ppx test1 \
  > 2> /dev/null | \
  > jq '.value.ocaml.ppx'
  [
    {
      "workdir": "$TESTCASE_ROOT",
      "workval": "test1"
    }
  ]

Neither is an absolute path

  $ echo | $MERLIN single dump-configuration -filename relative_path.ml -ppx /test2 \
  > 2> /dev/null | \
  > jq '.value.ocaml.ppx'
  [
    {
      "workdir": "$TESTCASE_ROOT",
      "workval": "/test2"
    }
  ]

But relative names are

  $ echo | $MERLIN single dump-configuration -filename relative_path.ml -ppx ./test3 \
  > 2> /dev/null | \
  > jq '.value.ocaml.ppx'
  [
    {
      "workdir": "$TESTCASE_ROOT",
      "workval": "./test3"
    }
  ]
