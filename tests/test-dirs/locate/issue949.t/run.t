This test is for testing the behavior of identifiers with a . in them:

  $ $MERLIN single locate -look-for ml -position 2:16 \
  > -filename ./issue949.ml < ./issue949.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/issue949.ml",
      "pos": {
        "line": 1,
        "col": 22
      }
    },
    "notifications": []
  }
