
  $ $MERLIN single locate -look-for ml -position 2:13 -filename ./mod_constr.ml < ./mod_constr.ml | sed 's/"[^""]*string.ml"/"irrelevant_path\/string.ml"/'
  {
    "class": "return",
    "value": {
      "file": "irrelevant_path/string.ml",
      "pos": {
        "line": 1,
        "col": 0
      }
    },
    "notifications": []
  }
