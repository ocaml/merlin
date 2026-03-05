Test case suggested in issue #1977

Label completion should work for inlined records as it does for standard
records.

  $ cat >inlined_record.ml <<'EOF'
  > type t = Constr of { some_lbl: int }
  > 
  > let foo (x : t) =
  >   match x with
  >   | Constr { som }
  > EOF

  $ $MERLIN single complete-prefix -position 5:16 -prefix 'some' \
  > -filename inlined_record.ml < ./inlined_record.ml | jq .value
  {
    "entries": [
      {
        "name": "some_lbl",
        "kind": "Label",
        "desc": "t.Constr -> int",
        "info": "",
        "deprecated": false
      }
    ],
    "context": null
  }

  $ cat >indirect.ml <<'EOF'
  > type r = { some_lbl: int }
  > type t = Constr of r
  > 
  > let foo (x : t) =
  >   match x with
  >   | Constr { som }
  > EOF

  $ $MERLIN single complete-prefix -position 6:16 -prefix 'some' \
  > -filename indirect.ml < ./indirect.ml | jq .value
  {
    "entries": [
      {
        "name": "some_lbl",
        "kind": "Label",
        "desc": "r -> int",
        "info": "",
        "deprecated": false
      }
    ],
    "context": null
  }
