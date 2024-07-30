  $ cat >foo.ml <<EOF
  > type a = A | B of int * b
  > and b = a list
  > EOF

FIXME: There is no stackoverflow anymore, but the results are still unsatisfying
We might need to rely on a latter environment from after the type definition to
provide better result.

  $ $MERLIN single type-enclosing -position 1:24 -verbosity 1 \
  > -filename ./foo.ml < ./foo.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 24
        },
        "end": {
          "line": 1,
          "col": 25
        },
        "type": "type b = 'a",
        "tail": "no"
      },
      {
        "start": {
          "line": 1,
          "col": 24
        },
        "end": {
          "line": 1,
          "col": 25
        },
        "type": "'a",
        "tail": "no"
      },
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 1,
          "col": 25
        },
        "type": "type a = A | B of int * b",
        "tail": "no"
      }
    ],
    "notifications": []
  }
