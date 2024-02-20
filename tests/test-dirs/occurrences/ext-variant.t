See issue #1185 on vscode-ocaml-platform

  $ cat >main.ml <<EOF
  > (*type t = ..*)
  > type t  = A
  > 
  > let foo (x : t) = match x with
  > | A -> 1
  > | _ -> 0
  > EOF

  $ $MERLIN single occurrences -identifier-at 5:2 \
  > -filename main.ml <main.ml | jq '.value'
  [
    {
      "start": {
        "line": 2,
        "col": 10
      },
      "end": {
        "line": 2,
        "col": 11
      }
    },
    {
      "start": {
        "line": 5,
        "col": 2
      },
      "end": {
        "line": 5,
        "col": 3
      }
    }
  ]


  $ cat >main.ml <<EOF
  > type t = ..
  > type t += A
  > 
  > let foo (x : t) = match x with
  > | A -> 1
  > | _ -> 0
  > EOF

FIXME: we can do better than that
  $ $MERLIN single occurrences -identifier-at 5:2 \
  > -filename main.ml <main.ml | jq '.value'
  [
    {
      "start": {
        "line": 2,
        "col": 10
      },
      "end": {
        "line": 2,
        "col": 11
      }
    },
    {
      "start": {
        "line": 5,
        "col": 2
      },
      "end": {
        "line": 5,
        "col": 3
      }
    }
  ]

