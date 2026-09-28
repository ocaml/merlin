  $ cat >test.ml <<'EOF'
  > module F (_ : sig end) = struct
  >   module I = struct
  >     type t = { lbl : int } 
  >   end
  > end
  > module M = struct end
  > module X = F(M)
  > let x = let open F(M) in { I.lbl = 42 }
  > let _ = match (x : F(M).I.t) with
  >   | X.I.{ lbl } -> ignore lbl
  > EOF

We expect module appearing in paths to be highlighted

F:
  $ $MERLIN single occurrences  -identifier-at 1:7 -filename test.ml <test.ml | 
  > jq '.value'
  [
    {
      "start": {
        "line": 1,
        "col": 7
      },
      "end": {
        "line": 1,
        "col": 8
      },
      "stale": false
    },
    {
      "start": {
        "line": 7,
        "col": 11
      },
      "end": {
        "line": 7,
        "col": 12
      },
      "stale": false
    },
    {
      "start": {
        "line": 8,
        "col": 17
      },
      "end": {
        "line": 8,
        "col": 18
      },
      "stale": false
    },
    {
      "start": {
        "line": 9,
        "col": 19
      },
      "end": {
        "line": 9,
        "col": 20
      },
      "stale": false
    }
  ]


I:
  $ $MERLIN single occurrences  -identifier-at 2:9 -filename test.ml <test.ml | 
  > jq '.value'
  [
    {
      "start": {
        "line": 2,
        "col": 9
      },
      "end": {
        "line": 2,
        "col": 10
      },
      "stale": false
    },
    {
      "start": {
        "line": 8,
        "col": 27
      },
      "end": {
        "line": 8,
        "col": 28
      },
      "stale": false
    },
    {
      "start": {
        "line": 9,
        "col": 24
      },
      "end": {
        "line": 9,
        "col": 25
      },
      "stale": false
    },
    {
      "start": {
        "line": 10,
        "col": 6
      },
      "end": {
        "line": 10,
        "col": 7
      },
      "stale": false
    }
  ]

t:
  $ $MERLIN single occurrences  -identifier-at 3:9 -filename test.ml <test.ml | 
  > jq '.value'
  [
    {
      "start": {
        "line": 3,
        "col": 9
      },
      "end": {
        "line": 3,
        "col": 10
      },
      "stale": false
    },
    {
      "start": {
        "line": 9,
        "col": 26
      },
      "end": {
        "line": 9,
        "col": 27
      },
      "stale": false
    }
  ]

lbl:
  $ $MERLIN single occurrences  -identifier-at 3:15 -filename test.ml <test.ml | 
  > jq '.value'
  [
    {
      "start": {
        "line": 3,
        "col": 15
      },
      "end": {
        "line": 3,
        "col": 18
      },
      "stale": false
    },
    {
      "start": {
        "line": 8,
        "col": 29
      },
      "end": {
        "line": 8,
        "col": 32
      },
      "stale": false
    },
    {
      "start": {
        "line": 10,
        "col": 10
      },
      "end": {
        "line": 10,
        "col": 13
      },
      "stale": false
    }
  ]



