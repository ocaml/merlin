Test for issue 2113: duplicate occurrence on recovered let binding

  $ $MERLIN single occurrences -identifier-at 1:9 -filename ./main.ml <<EOF
  > let rec ma
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 8
        },
        "end": {
          "line": 1,
          "col": 10
        },
        "stale": false
      },
      {
        "start": {
          "line": 1,
          "col": 8
        },
        "end": {
          "line": 1,
          "col": 10
        },
        "stale": false
      }
    ],
    "notifications": []
  }

