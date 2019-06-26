Searching foo from bar works:

  $ $MERLIN single locate -look-for ml -position 8:6 -filename ./issue973.ml < ./issue973.ml
  {
    "class": "return",
    "value": {
      "file": "tests/locate/mutually-recursive/issue973.ml",
      "pos": {
        "line": 1,
        "col": 8
      }
    },
    "notifications": []
  }

But bar from foo doesn't:

  $ $MERLIN single locate -look-for ml -position 2:7 -filename ./issue973.ml < ./issue973.ml
  {
    "class": "return",
    "value": "didn't manage to find bar",
    "notifications": []
  }
