Jump on the argument passed to the functor:
FIXME: we confuse the module for the constructor and jump to the wrong place

  $ $MERLIN single locate -look-for ml -position 11:18 -filename ./from_application.ml < ./from_application.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/from_application.ml",
      "pos": {
        "line": 5,
        "col": 7
      }
    },
    "notifications": []
  }

Jump from inside the functor application to inside the functor application:

  $ $MERLIN single locate -look-for ml -position 18:16 -filename ./from_application.ml < ./from_application.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/from_application.ml",
      "pos": {
        "line": 14,
        "col": 9
      }
    },
    "notifications": []
  }

Jump from inside the functor application to the outer scope:

  $ $MERLIN single locate -look-for ml -position 14:18 -filename ./from_application.ml < ./from_application.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/from_application.ml",
      "pos": {
        "line": 9,
        "col": 5
      }
    },
    "notifications": []
  }

