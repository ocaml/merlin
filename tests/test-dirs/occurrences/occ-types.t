
  $ $MERLIN single occurrences -identifier-at 1:6 -filename type.ml <<EOF | \
  > jq '.value'
  > type t
  > type b = t
  > EOF
  [
    {
      "start": {
        "line": 1,
        "col": 5
      },
      "end": {
        "line": 1,
        "col": 6
      },
      "stale": false
    },
    {
      "start": {
        "line": 2,
        "col": 9
      },
      "end": {
        "line": 2,
        "col": 10
      },
      "stale": false
    }
  ]

  $ $MERLIN single occurrences -identifier-at 1:19 -filename type.ml <<EOF | \
  > jq '.value'
  > let f = fun (type t) (foo : t list) -> let (_ : t) = () in ()
  > EOF
  [
    {
      "start": {
        "line": 1,
        "col": 18
      },
      "end": {
        "line": 1,
        "col": 19
      },
      "stale": false
    },
    {
      "start": {
        "line": 1,
        "col": 28
      },
      "end": {
        "line": 1,
        "col": 29
      },
      "stale": false
    },
    {
      "start": {
        "line": 1,
        "col": 48
      },
      "end": {
        "line": 1,
        "col": 49
      },
      "stale": false
    }
  ]
