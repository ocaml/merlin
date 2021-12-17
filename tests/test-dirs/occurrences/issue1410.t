FIXME

First result is incorrect when in the body of a function with an optional argument

  $ $MERLIN single occurrences -identifier-at 3:3 -filename opt.ml <<EOF | \
  > jq '.value'
  > (* test case *)
  > let f ?(x=1) () = 2 ;;
  > None
  > EOF
  [
    {
      "start": {
        "line": 0,
        "col": -1
      },
      "end": {
        "line": 0,
        "col": -1
      }
    },
    {
      "start": {
        "line": 3,
        "col": 0
      },
      "end": {
        "line": 3,
        "col": 4
      }
    }
  ]

  $ $MERLIN single occurrences -identifier-at 3:3 -filename opt.ml <<EOF | \
  > jq '.value'
  > (* test case *)
  > let f () = 2 ;;
  > None
  > EOF
  [
    {
      "start": {
        "line": 3,
        "col": 0
      },
      "end": {
        "line": 3,
        "col": 4
      }
    }
  ]
