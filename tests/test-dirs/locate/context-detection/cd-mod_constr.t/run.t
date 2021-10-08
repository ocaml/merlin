  $ $MERLIN single locate -look-for ml -position 2:13 \
  > -filename ./mod_constr.ml < ./mod_constr.ml
  {
    "class": "return",
    "value": {
      "file": "lib/ocaml/string.ml",
      "pos": {
        "line": 1,
        "col": 0
      }
    },
    "notifications": []
  }
  $ $MERLIN single locate -look-for ml -position 2:18 \
  > -filename ./mod_constr.ml < ./mod_constr.ml
  {
    "class": "return",
    "value": {
      "file": "lib/ocaml/string.ml",
      "pos": {
        "line": 72,
        "col": 4
      }
    },
    "notifications": []
  }
