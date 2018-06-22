  $ $MERLIN single phrase next -position 1:0 -filename test.ml <<EOF \
  > let x = 5 \
  > let y = 2 \
  > EOF
  {
    "class": "return",
    "value": {
      "pos": {
        "line": 2,
        "col": 0
      }
    },
    "notifications": []
  }

FIXME: ??

  $ $MERLIN single phrase prev -position 2:0 -filename test.ml <<EOF \
  > let x = 5 \
  > let y = 2 \
  > EOF
  {
    "class": "return",
    "value": {
      "pos": {
        "line": 3,
        "col": 0
      }
    },
    "notifications": []
  }
