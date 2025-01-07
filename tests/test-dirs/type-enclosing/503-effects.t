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

FIXME: the continuation is invisible to Merlin
  $ $MERLIN single type-enclosing -position 10:16 \
  > -filename main.ml <main.ml | jq '.value[0]'
  {
    "start": {
      "line": 8,
      "col": 4
    },
    "end": {
      "line": 10,
      "col": 23
    },
    "type": "int",
    "tail": "no"
  }
