
  $ $MERLIN single locate -look-for ml -position 4:11 -filename ./label.ml < ./label.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/label.ml",
      "pos": {
        "line": 3,
        "col": 4
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 9:11 -filename ./label.ml < ./label.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/label.ml",
      "pos": {
        "line": 8,
        "col": 4
      }
    },
    "notifications": []
  }

