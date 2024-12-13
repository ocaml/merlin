  $ dune build @check 

When the definition is in one of the implicit transitive dependencies
Merlin does not found the file in the source path provided by Dune. 
This works as expected since Dune lang 3.17 and OCaml >= 5.2
  $ $MERLIN single locate -look-for ml -position 1:15 \
  > -filename bin/main.ml <bin/main.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/src/lib2/lib2.ml",
      "pos": {
        "line": 1,
        "col": 5
      }
    },
    "notifications": []
  }
