test

  $ $MERLIN single type-enclosing -position 1:18 -filename inside_decl.ml \
  > -log-section type-enclosing -log-file - <<EOF 2>&1 | grep -v "^#"
  > type t1 = [ \`A of t1 | \`B ]
  > EOF
  paths: [t1]
  [
    {
      "start": { "line": 1, "col": 18 },
      "end": { "line": 1, "col": 20 },
      "identifier": "t1"
    }
  ]
  node = core_type
  exprs = [t1]
  source = t1; context = type
  typed t1
  result = [ File "inside_decl.ml", line 1, characters 18-20 ]
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 18
        },
        "end": {
          "line": 1,
          "col": 20
        },
        "type": "type t1 = 'a",
        "tail": "no"
      },
      {
        "start": {
          "line": 1,
          "col": 18
        },
        "end": {
          "line": 1,
          "col": 20
        },
        "type": "t1",
        "tail": "no"
      },
      {
        "start": {
          "line": 1,
          "col": 10
        },
        "end": {
          "line": 1,
          "col": 27
        },
        "type": "[ `A of t1 | `B ]",
        "tail": "no"
      },
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 1,
          "col": 27
        },
        "type": "type t1 = [ `A of t1 | `B ]",
        "tail": "no"
      }
    ],
    "notifications": []
  }

test
