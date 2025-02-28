Test finding occurrences of let-based binding operator, from reified syntax:
  $ $MERLIN single occurrences -identifier-at 3:11 ./issue1398.ml < ./issue1398.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 4
        },
        "end": {
          "line": 1,
          "col": 11
        },
        "stale": false
      },
      {
        "start": {
          "line": 3,
          "col": 10
        },
        "end": {
          "line": 3,
          "col": 17
        },
        "stale": false
      },
      {
        "start": {
          "line": 4,
          "col": 0
        },
        "end": {
          "line": 4,
          "col": 5
        },
        "stale": false
      }
    ],
    "notifications": []
  }

Test finding occurrences of and-based binding operator, from reified syntax:

  $ $MERLIN single occurrences -identifier-at 3:20 ./issue1398.ml < ./issue1398.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 4
        },
        "end": {
          "line": 2,
          "col": 11
        },
        "stale": false
      },
      {
        "start": {
          "line": 3,
          "col": 19
        },
        "end": {
          "line": 3,
          "col": 26
        },
        "stale": false
      },
      {
        "start": {
          "line": 4,
          "col": 12
        },
        "end": {
          "line": 4,
          "col": 17
        },
        "stale": false
      }
    ],
    "notifications": []
  }

  $ $MERLIN single occurrences -identifier-at 4:0 ./issue1398.ml < ./issue1398.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 4
        },
        "end": {
          "line": 1,
          "col": 11
        },
        "stale": false
      },
      {
        "start": {
          "line": 3,
          "col": 10
        },
        "end": {
          "line": 3,
          "col": 17
        },
        "stale": false
      },
      {
        "start": {
          "line": 4,
          "col": 0
        },
        "end": {
          "line": 4,
          "col": 5
        },
        "stale": false
      }
    ],
    "notifications": []
  }

  $ $MERLIN single occurrences -identifier-at 4:12 ./issue1398.ml < ./issue1398.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 4
        },
        "end": {
          "line": 2,
          "col": 11
        },
        "stale": false
      },
      {
        "start": {
          "line": 3,
          "col": 19
        },
        "end": {
          "line": 3,
          "col": 26
        },
        "stale": false
      },
      {
        "start": {
          "line": 4,
          "col": 12
        },
        "end": {
          "line": 4,
          "col": 17
        },
        "stale": false
      }
    ],
    "notifications": []
  }
