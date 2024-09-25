  $ dune build @check

When the deifinition is in one of the implicit transitive dependencies Merlin
used to not find the file in the source path provided by Dune. 
  $ $MERLIN single locate -look-for ml -position 1:15 \
  > -filename bin/main.ml <bin/main.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/src/lib2/lib2.ml",
      "pos": {
        "line": 1,
        "col": 0
      }
    },
    "notifications": []
  }
