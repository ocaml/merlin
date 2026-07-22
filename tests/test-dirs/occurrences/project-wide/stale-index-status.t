Merlin checks the files listed in the index against the files on disk, and
collects the files that were modified since the index was built, to report
that occurrences might be out-of-sync. The result of these checks used to be
read *before* the checks were performed (tuple components are evaluated
right-to-left), so out-of-sync files were never reported. The reported files
are observable in the logs.

  $ cat >lib.ml <<'EOF'
  > (* blah *)
  > let foo = "bar"
  > EOF

  $ cat >main.ml <<'EOF'
  > let () = print_string Lib.foo
  > EOF

  $ $OCAMLC -bin-annot -bin-annot-occurrences -c lib.ml main.ml
  $ ocaml-index aggregate main.cmt lib.cmt

With a fresh index, no file is reported as out-of-sync:
  $ $MERLIN single occurrences -scope project -identifier-at 1:28 \
  > -index-file project.ocaml-index \
  > -log-file - -log-section occurrences \
  > -filename main.ml < main.ml 2>&1 | grep "out-of-sync"
  [1]

Modify lib.ml without rebuilding the index: foo is now defined on line 1
instead of line 2, and the file's size changed.
  $ cat >lib.ml <<'EOF'
  > let foo = "bar"
  > EOF

The stat-check detects the modification and the query reports lib.ml as
out-of-sync:
  $ $MERLIN single occurrences -scope project -identifier-at 1:28 \
  > -index-file project.ocaml-index \
  > -log-file - -log-section occurrences \
  > -filename main.ml < main.ml 2>&1 | grep "out-of-sync"
  File $TESTCASE_ROOT/lib.ml might be out-of-sync.
  Occurrences may be incomplete: some source files are out-of-sync with the index: lib.ml
