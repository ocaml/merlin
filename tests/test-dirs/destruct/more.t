FIXME: `Some 0` certainly is a missing case but we can do better

  $ $MERLIN single case-analysis -start 3:4 -end 3:8 -filename complete.ml -log-file /tmp/mlog2 <<EOF \
  > let _ = \
  >   match (None : int option) with \
  >   | Some 3 -> () \
  >   | exception _ -> () \
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 21
        },
        "end": {
          "line": 4,
          "col": 21
        }
      },
      "
  | Some 0|None -> (??)"
    ],
    "notifications": []
  }
