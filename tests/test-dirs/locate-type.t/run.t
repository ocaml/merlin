  $ $OCAMLC b.ml -bin-annot -c

  $ $MERLIN single locate-type -position 6:6 -filename ./a.ml < ./a.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/a.ml",
      "pos": {
        "line": 2,
        "col": 7
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate-type -workdir . -position 9:11 -filename a.ml < ./a.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/a.ml",
      "pos": {
        "line": 2,
        "col": 7
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate-type -position 11:12 -filename ./a.ml < ./a.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/b.ml",
      "pos": {
        "line": 1,
        "col": 5
      }
    },
    "notifications": []
  }
