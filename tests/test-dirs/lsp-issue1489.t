  $ cat >repro.ml <<'EOF'
  > type t = ( :: )
  > let f (x: t) =  x
  > EOF

This should not hang and return a matching.
  $ $MERLIN single case-analysis -start 2:16 -end 2:17 \
  > -filename repro.ml <repro.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 16
        },
        "end": {
          "line": 2,
          "col": 17
        }
      },
      "match x with | (::) -> _"
    ],
    "notifications": []
  }
