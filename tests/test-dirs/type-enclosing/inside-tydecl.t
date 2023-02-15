test

  $ $MERLIN single type-enclosing -position 1:18 -filename inside_decl.ml \
  > -log-section type-enclosing -log-file - <<EOF
  > type t1 = [ \`A of t1 | \`B ]
  > EOF
  # 0.01 type-enclosing - reconstruct-identifier
  paths: [t1]
  # 0.01 type-enclosing - reconstruct identifier
  [
    { "start": { "line": 1, "col": 18 }, "end": { "line": 1, "col": 20 },
    "identifier": "t1" }
  ]
  # 0.01 type-enclosing - from_reconstructed
  node = core_type
  exprs = [t1]
  # 0.01 type-enclosing - from_reconstructed
  source = t1; context = type
  # 0.01 type-enclosing - from_reconstructed
  typed t1
  # 0.01 type-enclosing - small enclosing
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
        "type": "type t1 = t1",
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
