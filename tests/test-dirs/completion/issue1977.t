Test case suggested in issue #1977

FIXME: Label completion should work for inlined records as it does for standard
records.

  $ cat >inlined_record.ml <<'EOF'
  > type t = Constr of { some_lbl: int }
  > 
  > let foo (x : t) =
  >   match x with
  >   | Constr { som }
  > EOF

DBG: after printing the logs it feels like the issue is due to the special paths
used for inline records types. 

DBG: the additional debug print shows that the inlined records' label are not
found when calling Env.fold_labels.

  $ $MERLIN single complete-prefix -position 5:16 -prefix 'some' \
  > -log-file - -log-section Completion \
  > -filename inlined_record.ml < ./inlined_record.ml | jq .value
  # 0.02 Completion - branch_complete
  Leaf node: pattern (inlined_record.ml[5,71+11]..inlined_record.ml[5,71+18])
    attribute "merlin.incorrect"
      []
    Tpat_any
  # 0.02 Completion - branch_complete
  Common case, prefix = some, is_label = false
  # 0.02 Completion - find
  prefix = some
  # 0.02 Completion - fold_sumtype_constructors
  node type: t.Constr
  # 0.02 Completion - get_candidate
  Labels for prefix=some prefix_path=
  # 0.02 Completion - get_candidate
  Found label contents in env
  {
    "entries": [],
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
  > -log-file - -log-section Completion \
  > -filename indirect.ml < ./indirect.ml | jq .value
  # 0.01 Completion - branch_complete
  Leaf node: pattern (indirect.ml[6,82+11]..indirect.ml[6,82+18])
    attribute "merlin.incorrect"
      []
    Tpat_any
  # 0.01 Completion - branch_complete
  Common case, prefix = some, is_label = false
  # 0.01 Completion - find
  prefix = some
  # 0.01 Completion - fold_sumtype_constructors
  node type: r
  # 0.01 Completion - get_candidate
  Labels for prefix=some prefix_path=
  # 0.01 Completion - get_candidate
  Found label some_lbl in env
  # 0.01 Completion - get_candidate
  Found label contents in env
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
