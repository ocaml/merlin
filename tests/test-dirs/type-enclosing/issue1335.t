
  $ cat >foo.ml <<EOF
  > type 'k t = 'k baz
  > and 'k baz =
  >   | Foo
  >   | Bar of 'k t
  > EOF

FIXME: There is no stackoverflow anymore, but the results are still unsatisfying
We might need to rely on a latter environment from after the type definition to
provide better result.

  $ $MERLIN single type-enclosing -position 4:15 -verbosity 1 \
  > -filename ./foo.ml < ./foo.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 14
        },
        "end": {
          "line": 4,
          "col": 15
        },
        "type": "type 'a t = 'b",
        "tail": "no"
      },
      {
        "start": {
          "line": 4,
          "col": 11
        },
        "end": {
          "line": 4,
          "col": 15
        },
        "type": "'a",
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 0
        },
        "end": {
          "line": 4,
          "col": 15
        },
        "type": "type 'k baz = Foo | Bar of 'k t",
        "tail": "no"
      }
    ],
    "notifications": []
  }

  $ cat >foo.ml <<EOF
  > type t1 = [\`A of t1 | \`B]
  > EOF


FIXME: There is no stackoverflow anymore, but the results are still unsatisfying
We might need to rely on a latter environment from after the type definition to
provide better result.
  $ $MERLIN single type-enclosing -position 1:12 -verbosity 1 \
  > -filename ./foo.ml < ./foo.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 10
        },
        "end": {
          "line": 1,
          "col": 25
        },
        "type": "[ `A of 'a | `B ]",
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
        "type": "type t1 = [ `A of [ `A of 'a | `B ] as 'a | `B ]",
        "tail": "no"
      }
    ],
    "notifications": []
  }
