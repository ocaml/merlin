  $ cat >main.ml <<'EOF'
  > let rec foo x = x + a + b
  > and (a,b) = (1,2)
  > EOF

  $ $MERLIN single type-enclosing -position 1:16 -verbosity 0 \
  > -filename ./main.ml < ./main.ml | jq '.value[0]'
  {
    "start": {
      "line": 1,
      "col": 16
    },
    "end": {
      "line": 1,
      "col": 17
    },
    "type": "int",
    "tail": "no"
  }

  $ $MERLIN single type-enclosing -position 2:15 -verbosity 0 \
  > -filename ./main.ml < ./main.ml | jq '.value[0]'
  {
    "start": {
      "line": 2,
      "col": 15
    },
    "end": {
      "line": 2,
      "col": 16
    },
    "type": "int",
    "tail": "no"
  }

  $ $MERLIN single errors \
  > -filename ./main.ml < ./main.ml | jq '.value[].message'
  "Only variables are allowed as left-hand side of let rec"

  $ cat >main.ml <<'EOF'
  > let rec (x : unit) = ignore x
  > EOF

  $ $MERLIN single type-enclosing -position 1:28 -verbosity 0 \
  > -filename ./main.ml < ./main.ml | jq '.value[0]'
  {
    "start": {
      "line": 1,
      "col": 28
    },
    "end": {
      "line": 1,
      "col": 29
    },
    "type": "unit",
    "tail": "no"
  }

  $ $MERLIN single errors \
  > -filename ./main.ml < ./main.ml | jq '.value[].message'
  "This kind of expression is not allowed as right-hand side of let rec"
