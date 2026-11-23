From issue 2112:
https://github.com/ocaml/merlin/issues/2112

  $ echo "object method x with|0" > test.ml

FIXME: destruct should not fail with an assertion failure.
  $ $MERLIN single case-analysis -start 1:21 -end 1:21 \
  > -filename test.ml <test.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 22
        },
        "end": {
          "line": 1,
          "col": 22
        }
      },
      "
  | 1 -> _"
    ],
    "notifications": []
  }
