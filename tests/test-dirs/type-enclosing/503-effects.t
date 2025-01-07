  $ cat >main.ml <<'EOF'
  > open Effect
  > open Effect.Deep
  > 
  > type _ eff += E : unit eff
  > 
  > let () =
  >   Printf.printf "%d\n%!" @@
  >     match 10 with
  >     | x -> x
  >     | effect E, k -> 11
  > EOF

  $ $MERLIN single errors -filename main.ml <main.ml
  {
    "class": "return",
    "value": [],
    "notifications": []
  }

  $ $MERLIN single type-enclosing -position 10:13 \
  > -filename main.ml <main.ml | jq '.value[0]'
  {
    "start": {
      "line": 10,
      "col": 13
    },
    "end": {
      "line": 10,
      "col": 14
    },
    "type": "unit eff",
    "tail": "no"
  }

The continuation is visible to Merlin
  $ $MERLIN single type-enclosing -position 10:16 \
  > -filename main.ml <main.ml | jq '.value[0]'
  {
    "start": {
      "line": 10,
      "col": 16
    },
    "end": {
      "line": 10,
      "col": 17
    },
    "type": "(%eff, int) continuation",
    "tail": "no"
  }
