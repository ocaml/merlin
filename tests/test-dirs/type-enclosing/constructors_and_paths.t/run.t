Various parts of the cons.ml:

- The expression:
  $ $MERLIN single type-enclosing -position 4:14 -verbosity 0 \
  > -filename ./cons.ml < ./cons.ml| jq ".value[0:2]"
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
    }
  ]

- The pattern:

  $ $MERLIN single type-enclosing -position 8:5 -verbosity 0 \
  > -filename ./cons.ml < ./cons.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 8,
        "col": 4
      },
      "end": {
        "line": 8,
        "col": 5
      },
      "type": "t",
      "tail": "no"
    },
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
    }
  ]

- Non-regression tests:

  $ $MERLIN single type-enclosing -position 17:9 -verbosity 0 \
  > -filename ./cons.ml < ./cons.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 17,
        "col": 8
      },
      "end": {
        "line": 17,
        "col": 9
      },
      "type": "sig type t = A type u = A | B end",
      "tail": "no"
    },
    {
      "start": {
        "line": 17,
        "col": 8
      },
      "end": {
        "line": 17,
        "col": 11
      },
      "type": "M.u",
      "tail": "no"
    }
  ]

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
        "col": 6
      },
      "end": {
        "line": 15,
        "col": 22
      },
      "type": "unit -> M.t",
      "tail": "no"
    }
  ]

FIXME: the following two tests work only because of
the fallbacks implemented in type_utils. Context is
unable to answer correctly due to the enclosing node
being "Texp_constant" and not "Texp_construct". in
the expression reconstructed from  (M|.A 3).
  $ $MERLIN single type-enclosing -position 24:15 -verbosity 0 \
  > -filename ./cons.ml < ./cons.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 24,
        "col": 14
      },
      "end": {
        "line": 24,
        "col": 15
      },
      "type": "sig type t = A of int val x : int end",
      "tail": "no"
    },
    {
      "start": {
        "line": 24,
        "col": 13
      },
      "end": {
        "line": 24,
        "col": 20
      },
      "type": "N.t",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 24:17 -verbosity 0 \
  > -filename ./cons.ml < ./cons.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 24,
        "col": 14
      },
      "end": {
        "line": 24,
        "col": 17
      },
      "type": "int -> N.t",
      "tail": "no"
    },
    {
      "start": {
        "line": 24,
        "col": 13
      },
      "end": {
        "line": 24,
        "col": 20
      },
      "type": "N.t",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 26:9 -verbosity 0 \
  >  -filename ./cons.ml < ./cons.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 26,
        "col": 8
      },
      "end": {
        "line": 26,
        "col": 9
      },
      "type": "sig type t = A of int val x : int end",
      "tail": "no"
    },
    {
      "start": {
        "line": 26,
        "col": 8
      },
      "end": {
        "line": 26,
        "col": 11
      },
      "type": "int",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 26:11 -verbosity 0 \
  > -filename ./cons.ml < ./cons.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 26,
        "col": 8
      },
      "end": {
        "line": 26,
        "col": 11
      },
      "type": "int",
      "tail": "no"
    }
  ]
