  $ cat >test.ml <<'EOF'
  > type t = A of (int * string)
  > let f (A _) = 5
  > EOF

  $ $MERLIN single type-enclosing -position 2:9 \
  > -filename test.ml <test.ml | jq '.value[0].type'
  "int * string"

  $ cat >test.ml <<'EOF'
  > type t = A of int * string
  > let f (A _) = 5
  > EOF

The type of all the constructor arguments is shown
  $ $MERLIN single type-enclosing -position 2:9 \
  > -filename test.ml <test.ml | jq '.value[0].type'
  "int * string"

  $ $MERLIN single case-analysis -start 2:9 -end 2:9 \
  > -filename test.ml <test.ml | jq '.value'
  [
    {
      "start": {
        "line": 2,
        "col": 9
      },
      "end": {
        "line": 2,
        "col": 10
      }
    },
    "(_, _)"
  ]

  $ cat >test.ml <<'EOF'
  > type t = A of int * string * bool
  > let f (A (_, _, _)) = 5
  > EOF

Explicit wildcards are not affected
  $ $MERLIN single type-enclosing -position 2:10 \
  > -filename test.ml <test.ml | jq '.value[0].type'
  "int"
