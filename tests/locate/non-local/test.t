Test that Locate.locate and Locate.from_path do their job properly:

  $ $MERLIN single locate -look-for ml -position 1:11 -filename ./b.ml < ./b.ml
  {
    "class": "return",
    "value": {
      "file": "tests/locate/non-local/a.ml",
      "pos": {
        "line": 1,
        "col": 4
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 5:15 -filename ./b.ml < ./b.ml
  {
    "class": "return",
    "value": {
      "file": "tests/locate/non-local/a.ml",
      "pos": {
        "line": 1,
        "col": 4
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 9:9 -filename ./b.ml < ./b.ml
  {
    "class": "return",
    "value": {
      "file": "tests/locate/non-local/a.ml",
      "pos": {
        "line": 1,
        "col": 4
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for mli -position 1:11 -filename ./b.ml < ./b.ml
  {
    "class": "return",
    "value": {
      "file": "tests/locate/non-local/a.mli",
      "pos": {
        "line": 3,
        "col": 0
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for mli -position 5:15 -filename ./b.ml < ./b.ml
  {
    "class": "return",
    "value": {
      "file": "tests/locate/non-local/a.mli",
      "pos": {
        "line": 3,
        "col": 0
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for mli -position 9:9 -filename ./b.ml < ./b.ml
  {
    "class": "return",
    "value": {
      "file": "tests/locate/non-local/a.mli",
      "pos": {
        "line": 3,
        "col": 0
      }
    },
    "notifications": []
  }

