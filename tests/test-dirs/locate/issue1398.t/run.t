Test locating definition of let-based binding operator, from reified syntax:

  $ $MERLIN single locate -position 3:11 ./issue1398.ml < ./issue1398.ml
  {
    "class": "return",
    "value": {
      "file": "*buffer*",
      "pos": {
        "line": 1,
        "col": 4
      }
    },
    "notifications": []
  }

Test locating definition of and-based binding operator, from reified syntax:

  $ $MERLIN single locate -position 3:20 ./issue1398.ml < ./issue1398.ml
  {
    "class": "return",
    "value": {
      "file": "*buffer*",
      "pos": {
        "line": 2,
        "col": 4
      }
    },
    "notifications": []
  }

Test locating definition of let-based binding operator, from operator syntax:

  $ $MERLIN single locate -position 4:0 ./issue1398.ml < ./issue1398.ml
  {
    "class": "return",
    "value": {
      "file": "*buffer*",
      "pos": {
        "line": 1,
        "col": 4
      }
    },
    "notifications": []
  }

Test locating definition of and-based binding operator, from operator syntax:

  $ $MERLIN single locate -position 4:12 ./issue1398.ml < ./issue1398.ml
  {
    "class": "return",
    "value": {
      "file": "*buffer*",
      "pos": {
        "line": 2,
        "col": 4
      }
    },
    "notifications": []
  }
