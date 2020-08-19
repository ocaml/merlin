A simple name is not expanded

  $ echo | $MERLIN single dump-configuration -filename relative_path.ml -ppx test1 | \
  > jq '.value.ocaml.ppx'
  sh: 1: test1: not found
  [
    {
      "workdir": "$TESTCASE_ROOT",
      "workval": "test1"
    }
  ]

Neither is an absolute path

  $ echo | $MERLIN single dump-configuration -filename relative_path.ml -ppx /test2 | \
  > jq '.value.ocaml.ppx'
  sh: 1: /test2: not found
  [
    {
      "workdir": "$TESTCASE_ROOT",
      "workval": "/test2"
    }
  ]

But relative names are

  $ echo | $MERLIN single dump-configuration -filename relative_path.ml -ppx ./test3 | \
  > jq '.value.ocaml.ppx'
  sh: 1: ./test3: not found
  [
    {
      "workdir": "$TESTCASE_ROOT",
      "workval": "./test3"
    }
  ]
