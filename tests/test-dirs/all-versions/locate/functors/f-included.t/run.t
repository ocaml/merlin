
  $ $MERLIN single locate -look-for ml -position 22:15 \
  > -filename ./included.ml < ./included.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/included.ml",
      "pos": {
        "line": 14,
        "col": 6
      }
    },
    "notifications": []
  }
