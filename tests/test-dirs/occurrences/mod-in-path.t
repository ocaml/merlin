  $ cat >test.ml <<'EOF'
  > module Mod = struct
  >   module Nod = struct
  >     let x = 42
  >   end
  > end
  > let _ = Mod.Nod.x
  > let _ = Mod . Nod . x
  > let _ = Mod  .
  > Nod 
  >   . 
  >   x
  > let _ = let open Mod in Nod.x
  > EOF

We expect module appearing in paths to be highlighted
  $ $MERLIN single occurrences -identifier-at 6:13 -filename test.ml <test.ml | 
  > jq '.value'
  [
    {
      "start": {
        "line": 2,
        "col": 9
      },
      "end": {
        "line": 2,
        "col": 12
      },
      "stale": false
    },
    {
      "start": {
        "line": 6,
        "col": 12
      },
      "end": {
        "line": 6,
        "col": 15
      },
      "stale": false
    },
    {
      "start": {
        "line": 7,
        "col": 14
      },
      "end": {
        "line": 7,
        "col": 17
      },
      "stale": false
    },
    {
      "start": {
        "line": 9,
        "col": 0
      },
      "end": {
        "line": 9,
        "col": 3
      },
      "stale": false
    },
    {
      "start": {
        "line": 12,
        "col": 24
      },
      "end": {
        "line": 12,
        "col": 27
      },
      "stale": false
    }
  ]

We expect module appearing in paths to be highlighted
  $ $MERLIN single occurrences -identifier-at 12:18 -filename test.ml <test.ml | 
  > jq '.value'
  [
    {
      "start": {
        "line": 1,
        "col": 7
      },
      "end": {
        "line": 1,
        "col": 10
      },
      "stale": false
    },
    {
      "start": {
        "line": 6,
        "col": 8
      },
      "end": {
        "line": 6,
        "col": 11
      },
      "stale": false
    },
    {
      "start": {
        "line": 7,
        "col": 8
      },
      "end": {
        "line": 7,
        "col": 11
      },
      "stale": false
    },
    {
      "start": {
        "line": 8,
        "col": 8
      },
      "end": {
        "line": 8,
        "col": 11
      },
      "stale": false
    },
    {
      "start": {
        "line": 12,
        "col": 17
      },
      "end": {
        "line": 12,
        "col": 20
      },
      "stale": false
    }
  ]
