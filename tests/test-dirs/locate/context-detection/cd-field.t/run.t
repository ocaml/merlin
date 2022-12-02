
  $ $MERLIN single locate -look-for ml -position 3:14 -filename ./field.ml < ./field.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/field.ml",
      "pos": {
        "line": 1,
        "col": 5
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
        "col": 5
      }
    },
    "notifications": []
  }

Merlin is confused by punned fields prefixed by the module. { X.bar } goes to
the field bar rather than the identifier.
  $ $MERLIN single locate -look-for ml -position 15:14 -filename ./field.ml < ./field.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/field.ml",
      "pos": {
        "line": 10,
        "col": 7
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
