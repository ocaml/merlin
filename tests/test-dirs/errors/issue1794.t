  $ $MERLIN single errors -filename main.ml -unboxed-types
  {
    "class": "return",
    "value": [],
    "notifications": []
  }

  $ $MERLIN single errors -filename main.ml -no-unboxed-types
  {
    "class": "return",
    "value": [],
    "notifications": []
  }
