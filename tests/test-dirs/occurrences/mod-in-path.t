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

FIXME: we could expect module appearing in paths to be highlighted
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
      }
    }
  ]

FIXME: we could expect module appearing in paths to be highlighted
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
      }
    },
    {
      "start": {
        "line": 12,
        "col": 17
      },
      "end": {
        "line": 12,
        "col": 20
      }
    }
  ]
