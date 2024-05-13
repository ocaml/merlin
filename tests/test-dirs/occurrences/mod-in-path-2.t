  $ cat >test.ml <<'EOF'
  > module Mod = struct
  >   type t = A 
  > end
  > let () = 
  >   match Mod.A with
  >   | Mod.A -> ()
  > EOF

  $ $MERLIN single occurrences -identifier-at 1:8 -filename test.ml <test.ml | 
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
        "line": 5,
        "col": 8
      },
      "end": {
        "line": 5,
        "col": 11
      }
    },
    {
      "start": {
        "line": 6,
        "col": 4
      },
      "end": {
        "line": 6,
        "col": 7
      }
    }
  ]

  $ $MERLIN single occurrences -identifier-at 2:11 -filename test.ml <test.ml | 
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
        "line": 5,
        "col": 12
      },
      "end": {
        "line": 5,
        "col": 13
      }
    },
    {
      "start": {
        "line": 6,
        "col": 8
      },
      "end": {
        "line": 6,
        "col": 9
      }
    }
  ]
