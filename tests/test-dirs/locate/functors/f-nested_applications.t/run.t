
  $ $MERLIN single locate -look-for ml -position 17:14 \
  > -filename ./nested_applications.ml < ./nested_applications.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/nested_applications.ml",
      "pos": {
        "line": 10,
        "col": 7
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 19:14 \
  > -filename ./nested_applications.ml < ./nested_applications.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/nested_applications.ml",
      "pos": {
        "line": 10,
        "col": 7
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 25:14 \
  > -filename ./nested_applications.ml < ./nested_applications.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/nested_applications.ml",
      "pos": {
        "line": 10,
        "col": 7
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 29:14 \
  > -filename ./nested_applications.ml < ./nested_applications.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/nested_applications.ml",
      "pos": {
        "line": 10,
        "col": 7
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 33:14 \
  > -filename ./nested_applications.ml < ./nested_applications.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/nested_applications.ml",
      "pos": {
        "line": 10,
        "col": 7
      }
    },
    "notifications": []
  }
