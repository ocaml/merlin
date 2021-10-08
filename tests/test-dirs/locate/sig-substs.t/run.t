Maybe-obsolete-note: we currently have no way to decide between a sig and a struct
when both are present in the buffer (the struct will always be preferred).

  $ $MERLIN single locate -look-for ml -position 10:12 -filename ./basic.ml < ./basic.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/basic.ml",
      "pos": {
        "line": 8,
        "col": 20
      }
    },
    "notifications": []
  }
