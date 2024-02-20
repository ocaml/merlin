FIXME: such substitutions are not handled properly yet.
On a similar note we currently have no way to decide between a sig and a struct
when both are present in the buffer (the struct will always be preferred).

  $ $MERLIN single locate -look-for ml -position 10:11 -filename ./basic.ml < ./basic.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/basic.ml",
      "pos": {
        "line": 8,
        "col": 9
      }
    },
    "notifications": []
  }

TODO SHAPES: it could be more precise by answering 8:21
  $ $MERLIN single locate -look-for ml -position 10:13 -filename ./basic.ml < ./basic.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/basic.ml",
      "pos": {
        "line": 8,
        "col": 25
      }
    },
    "notifications": []
  }
