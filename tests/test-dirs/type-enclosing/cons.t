Various parts of the cons.ml:

- The expression:
  $ $MERLIN single type-enclosing -position 4:14 -verbosity 0 \
  > -filename ./cons.ml < ./cons.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 4,
        "col": 13
      },
      "end": {
        "line": 4,
        "col": 14
      },
      "type": "t",
      "tail": "no"
    },
    {
      "start": {
        "line": 4,
        "col": 13
      },
      "end": {
        "line": 4,
        "col": 14
      },
      "type": "t",
      "tail": "no"
    }
  ]

Note: the output is duplicated because it is the result of the concatenation
of both the ast-based and the small_enclosings (source based) heuristics.
We aim to fix that in the future.

- The pattern:

  $ $MERLIN single type-enclosing -position 8:6 -verbosity 0 \
  > -filename ./cons.ml < ./cons.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 7,
        "col": 2
      },
      "end": {
        "line": 8,
        "col": 11
      },
      "type": "unit",
      "tail": "no"
    },
    {
      "start": {
        "line": 6,
        "col": 6
      },
      "end": {
        "line": 8,
        "col": 11
      },
      "type": "t -> unit",
      "tail": "no"
    }
  ]

- Non-regression test:

  $ $MERLIN single type-enclosing -position 15:13 -verbosity 0 \
  > -filename ./cons.ml < ./cons.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 15,
        "col": 12
      },
      "end": {
        "line": 15,
        "col": 13
      },
      "type": "sig type t = A type u = A | B end",
      "tail": "no"
    },
    {
      "start": {
        "line": 15,
        "col": 12
      },
      "end": {
        "line": 15,
        "col": 15
      },
      "type": "M.t",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 15:15 -verbosity 0 \
  > -filename ./cons.ml < ./cons.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 15,
        "col": 12
      },
      "end": {
        "line": 15,
        "col": 15
      },
      "type": "M.t",
      "tail": "no"
    },
    {
      "start": {
        "line": 15,
        "col": 12
      },
      "end": {
        "line": 15,
        "col": 15
      },
      "type": "M.t",
      "tail": "no"
    }
  ]
