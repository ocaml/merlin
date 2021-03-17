FIXME!

We would like this to say "Not in the environment b", because no label
declaration named b exists.
For this to work, we would need to know that the cursor is on a label name,
which we can't right now when looking on the recovered typedtree. We only know
we're in an expression.
If we switch our context analysis to work on the grammar, this might get better.
Until then ...

  $ $MERLIN single locate -look-for ml -position 2:10 -filename test.ml <<EOF
  > let b = 10
  > let x = { b = 9 }
  > EOF
  {
    "class": "return",
    "value": {
      "file": "test.ml",
      "pos": {
        "line": 1,
        "col": 4
      }
    },
    "notifications": []
  }
