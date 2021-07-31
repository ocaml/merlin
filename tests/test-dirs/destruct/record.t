Field projections should be handled the same way, regardless of whether we've
selected:

- the whole expression

  $ $MERLIN single case-analysis -start 2:2 -end 2:11 -filename test.ml <<EOF
  > let f (x : int ref) =
  >   x.contents
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 2
        },
        "end": {
          "line": 2,
          "col": 12
        }
      },
      "match x.contents with | 0 -> _ | _ -> _"
    ],
    "notifications": []
  }

- just the label

  $ $MERLIN single case-analysis -start 2:4 -end 2:11 -filename test.ml <<EOF
  > let f (x : int ref) =
  >   x.contents
  > EOF
  {
    "class": "error",
    "value": "Destruct not allowed on record_field",
    "notifications": []
  }
