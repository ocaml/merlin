
  $ $MERLIN single locate -look-for ml -position 3:14 -filename ./field.ml < ./field.ml
  {
    "class": "return",
    "value": {
      "file": "tests/locate/context-detection/field.ml",
      "pos": {
        "line": 1,
        "col": 0
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 7:14 -filename ./field.ml < ./field.ml
  {
    "class": "return",
    "value": {
      "file": "tests/locate/context-detection/field.ml",
      "pos": {
        "line": 1,
        "col": 0
      }
    },
    "notifications": []
  }

