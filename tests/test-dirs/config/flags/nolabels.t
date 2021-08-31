classic and labels

  $ $MERLIN single errors -filename labels_ok_1.ml <<EOF
  > let f ~x = () in f ~x:(); f ()
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 26
        },
        "end": {
          "line": 1,
          "col": 27
        },
        "type": "warning",
        "sub": [],
        "valid": true,
        "message": "Warning 6: label x was omitted in the application of this function."
      }
    ],
    "notifications": []
  }

  $ $MERLIN single errors -filename labels_ko_1.ml -nolabels <<EOF
  > let f ~x = () in f ~x:(); f ()
  > EOF
  {
    "class": "return",
    "value": [],
    "notifications": []
  }
