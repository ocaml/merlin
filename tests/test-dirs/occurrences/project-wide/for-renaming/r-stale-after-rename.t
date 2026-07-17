When a rename was applied to the sources but the project was not rebuilt yet,
the index and the cmt files still associate the identifier's uids with the old
name. Merlin then drops the related uids and can no longer find the
occurrences outside of the current buffer. It reports the files whose
artifacts are out-of-sync in the status of the query, so that clients can
detect the incomplete result instead of performing a partial rename.

Note that we deliberately rename [foo] to [bar] (same length) so that the
index's stat-check cannot detect the modification: only the name check can.

  $ cat >lib.mli <<'EOF'
  > val foo : int
  > EOF

  $ cat >lib.ml <<'EOF'
  > let foo = 42
  > let _ = foo
  > EOF

  $ cat >main.ml <<'EOF'
  > let () = print_int Lib.foo
  > EOF

  $ ocamlc -bin-annot -bin-annot-occurrences -c lib.mli lib.ml main.ml
  $ ocaml-index aggregate lib.cmti lib.cmt main.cmt

  $ locations () {
  >   $MERLIN single occurrences -scope renaming -identifier-at 1:4 \
  >   -index-file project.ocaml-index \
  >   -filename lib.mli <lib.mli \
  >   | jq -r '.value[] | "\(.file):\(.start.line):\(.start.col)"'
  > }

  $ show_incomplete () {
  >   $MERLIN single occurrences -scope renaming -identifier-at 1:4 \
  >   -index-file project.ocaml-index \
  >   -log-file - -log-section occurrences \
  >   -filename lib.mli <lib.mli 2>&1 | grep -o "Occurrences may be incomplete:.*"
  > }

With a fresh index, renaming [foo] from the interface finds the declaration,
the definition and all usages, show_incomplete does not return anything
  $ locations
  $TESTCASE_ROOT/lib.mli:1:4
  $TESTCASE_ROOT/lib.ml:1:4
  $TESTCASE_ROOT/lib.ml:2:8
  $TESTCASE_ROOT/main.ml:1:23
  $ show_incomplete
  [1]

Apply the edits of renaming [foo] to [bar], without rebuilding:
  $ cat >lib.mli <<'EOF'
  > val bar : int
  > EOF

  $ cat >lib.ml <<'EOF'
  > let bar = 42
  > let _ = bar
  > EOF

  $ cat >main.ml <<'EOF'
  > let () = print_int Lib.bar
  > EOF

Only the declaration of the current buffer is found
  $ locations
  $TESTCASE_ROOT/lib.mli:1:4
  $ show_incomplete
  Occurrences may be incomplete: some source files are out-of-sync with the index: lib.mli, lib.ml

After rebuilding, renaming from the interface works again:
  $ ocamlc -bin-annot -bin-annot-occurrences -c lib.mli lib.ml main.ml
  $ ocaml-index aggregate lib.cmti lib.cmt main.cmt
  $ locations
  $TESTCASE_ROOT/lib.mli:1:4
  $TESTCASE_ROOT/lib.ml:1:4
  $TESTCASE_ROOT/lib.ml:2:8
  $TESTCASE_ROOT/main.ml:1:23
  $ show_incomplete
  [1]
