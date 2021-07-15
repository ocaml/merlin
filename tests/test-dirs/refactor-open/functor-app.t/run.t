FIXME: functor applications in type paths are not handled

Unqualifying inside application paths:

  $ $MERLIN single refactor-open -action unqualify -position 26:10 \
  > -filename test.ml < test.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 28,
          "col": 44
        },
        "end": {
          "line": 28,
          "col": 53
        },
        "content": "C"
      },
      {
        "start": {
          "line": 29,
          "col": 12
        },
        "end": {
          "line": 29,
          "col": 24
        },
        "content": "Make"
      },
      {
        "start": {
          "line": 29,
          "col": 26
        },
        "end": {
          "line": 29,
          "col": 35
        },
        "content": "C"
      },
      {
        "start": {
          "line": 31,
          "col": 12
        },
        "end": {
          "line": 31,
          "col": 24
        },
        "content": "Make"
      },
      {
        "start": {
          "line": 36,
          "col": 19
        },
        "end": {
          "line": 36,
          "col": 28
        },
        "content": "C"
      },
      {
        "start": {
          "line": 37,
          "col": 13
        },
        "end": {
          "line": 37,
          "col": 25
        },
        "content": "Make"
      }
    ],
    "notifications": []
  }

Qualifying inside application paths:

  $ $MERLIN single refactor-open -action qualify -position 26:10 \
  > -filename test.ml < test.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 33,
          "col": 13
        },
        "end": {
          "line": 33,
          "col": 17
        },
        "content": "Wrapper.Make"
      },
      {
        "start": {
          "line": 33,
          "col": 19
        },
        "end": {
          "line": 33,
          "col": 20
        },
        "content": "Wrapper.C"
      },
      {
        "start": {
          "line": 34,
          "col": 13
        },
        "end": {
          "line": 34,
          "col": 17
        },
        "content": "Wrapper.Make"
      },
      {
        "start": {
          "line": 36,
          "col": 13
        },
        "end": {
          "line": 36,
          "col": 17
        },
        "content": "Wrapper.Make"
      },
      {
        "start": {
          "line": 37,
          "col": 27
        },
        "end": {
          "line": 37,
          "col": 28
        },
        "content": "Wrapper.C"
      }
    ],
    "notifications": []
  }
