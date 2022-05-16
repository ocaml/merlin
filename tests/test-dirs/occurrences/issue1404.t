FIXME there is a discrepancy on the detection of the expression under the cursor
between locate and occurrences.

occurrences identifier-at 2:0 returns the occurrences of [x]
  $ $MERLIN single occurrences -identifier-at 2:0 -filename opt.ml <<EOF | \
  > jq '.value'
  > let x = 3 and y = 4 + 2 in
  > x+y
  > EOF
  [
    {
      "start": {
        "line": 1,
        "col": 4
      },
      "end": {
        "line": 1,
        "col": 5
      }
    },
    {
      "start": {
        "line": 2,
        "col": 0
      },
      "end": {
        "line": 2,
        "col": 1
      }
    }
  ]

FIXME occurrences identifier-at 2:1 returns the occurrences of [x] (should be [+])
  $ $MERLIN single occurrences -identifier-at 2:1 -filename opt.ml <<EOF | \
  > jq '.value'
  > let x = 3 and y = 4 + 2 in
  > x+y
  > EOF
  [
    {
      "start": {
        "line": 1,
        "col": 4
      },
      "end": {
        "line": 1,
        "col": 5
      }
    },
    {
      "start": {
        "line": 2,
        "col": 0
      },
      "end": {
        "line": 2,
        "col": 1
      }
    }
  ]

locate position 2:0 returns the definition of [x]
  $ $MERLIN single locate -position 2:0 -filename opt.ml <<EOF | \
  > jq '.value'
  > let x = 3 and y = 4 + 2 in
  > x+y
  > EOF
  {
    "file": "opt.ml",
    "pos": {
      "line": 1,
      "col": 4
    }
  }


locate position 2:1 returns the definition of [(+)]
  $ $MERLIN single locate -position 2:1 -filename opt.ml <<EOF | \
  > jq '.value'
  > let x = 3 and y = 4 + 2 in
  > x+y
  > EOF
  {
    "file": "lib/ocaml/stdlib.mli",
    "pos": {
      "line": 347,
      "col": 0
    }
  }
