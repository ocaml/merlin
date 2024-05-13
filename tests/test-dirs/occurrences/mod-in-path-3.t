  $ cat >test.ml <<'EOF'
  > module Mod = struct
  >   type t = A of { lbl : int } 
  > end
  > let x = Mod.A { lbl = 42 }
  > let _ =
  >   match x with
  >   | Mod.A r -> r.lbl
  > EOF

  $ $MERLIN single occurrences -identifier-at 4:9 -filename test.ml <test.ml | 
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
      }
    },
    {
      "start": {
        "line": 4,
        "col": 8
      },
      "end": {
        "line": 4,
        "col": 11
      }
    },
    {
      "start": {
        "line": 7,
        "col": 4
      },
      "end": {
        "line": 7,
        "col": 7
      }
    }
  ]

  $ $MERLIN single occurrences -identifier-at 4:12 -filename test.ml <test.ml | 
  > jq '.value'
  [
    {
      "start": {
        "line": 2,
        "col": 11
      },
      "end": {
        "line": 2,
        "col": 12
      }
    },
    {
      "start": {
        "line": 4,
        "col": 12
      },
      "end": {
        "line": 4,
        "col": 13
      }
    },
    {
      "start": {
        "line": 7,
        "col": 8
      },
      "end": {
        "line": 7,
        "col": 9
      }
    }
  ]

  $ $MERLIN single occurrences -identifier-at 4:18 -filename test.ml <test.ml | 
  > jq '.value'
  [
    {
      "start": {
        "line": 2,
        "col": 18
      },
      "end": {
        "line": 2,
        "col": 21
      }
    },
    {
      "start": {
        "line": 4,
        "col": 16
      },
      "end": {
        "line": 4,
        "col": 19
      }
    },
    {
      "start": {
        "line": 7,
        "col": 17
      },
      "end": {
        "line": 7,
        "col": 20
      }
    }
  ]
