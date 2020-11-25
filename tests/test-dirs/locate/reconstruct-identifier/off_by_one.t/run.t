Regression test for #624

  $ $MERLIN single locate -look-for ml -position 4:13 -filename ./off_by_one.ml < ./off_by_one.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/off_by_one.ml",
      "pos": {
        "line": 1,
        "col": 0
      }
    },
    "notifications": []
  }
