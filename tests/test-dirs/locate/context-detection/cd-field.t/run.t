
  $ $MERLIN single locate -look-for ml -position 3:14 -filename ./field.ml < ./field.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/field.ml",
      "pos": {
        "line": 1,
        "col": 11
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 7:14 -filename ./field.ml < ./field.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/field.ml",
      "pos": {
        "line": 1,
        "col": 11
      }
    },
    "notifications": []
  }

Punned fields prefixed by the module go to the identifier, not the field:
  $ $MERLIN single locate -look-for ml -position 15:14 -filename ./field.ml < ./field.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/field.ml",
      "pos": {
        "line": 13,
        "col": 4
      }
    },
    "notifications": []
  }

Normal punning works as expected:
  $ $MERLIN single locate -look-for ml -position 15:19 -filename ./field.ml < ./field.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/field.ml",
      "pos": {
        "line": 14,
        "col": 4
      }
    },
    "notifications": []
  }
