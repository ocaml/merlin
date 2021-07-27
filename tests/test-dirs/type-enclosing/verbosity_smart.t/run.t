In the tests below we see the different behavior of verbosity = 0 and = "smart"

  $ $MERLIN single type-enclosing -position 13:9 -verbosity 0 \
  > -filename ./test.ml < ./test.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 13,
          "col": 7
        },
        "end": {
          "line": 13,
          "col": 9
        },
        "type": "(module M)",
        "tail": "no"
      },
      {
        "start": {
          "line": 13,
          "col": 0
        },
        "end": {
          "line": 13,
          "col": 13
        },
        "type": "(module M)",
        "tail": "no"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single type-enclosing -position 13:9 -verbosity smart \
  > -filename ./test.ml < ./test.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 13,
          "col": 7
        },
        "end": {
          "line": 13,
          "col": 9
        },
        "type": "sig val u : unit end",
        "tail": "no"
      },
      {
        "start": {
          "line": 13,
          "col": 0
        },
        "end": {
          "line": 13,
          "col": 13
        },
        "type": "sig val u : unit end",
        "tail": "no"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single type-enclosing -position 15:9 -verbosity 0 \
  > -filename ./test.ml < ./test.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 15,
          "col": 7
        },
        "end": {
          "line": 15,
          "col": 9
        },
        "type": "T",
        "tail": "no"
      },
      {
        "start": {
          "line": 15,
          "col": 0
        },
        "end": {
          "line": 15,
          "col": 16
        },
        "type": "T",
        "tail": "no"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single type-enclosing -position 15:9 -verbosity smart \
  > -filename ./test.ml < ./test.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 15,
          "col": 7
        },
        "end": {
          "line": 15,
          "col": 9
        },
        "type": "sig val u : unit end",
        "tail": "no"
      },
      {
        "start": {
          "line": 15,
          "col": 0
        },
        "end": {
          "line": 15,
          "col": 16
        },
        "type": "sig val u : unit end",
        "tail": "no"
      }
    ],
    "notifications": []
  }
