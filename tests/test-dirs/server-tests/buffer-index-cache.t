  $ cat >test.ml <<'EOF'
  > let x = 36 
  > let y = 
  >   let z = x + x in
  >   z + x
  > EOF

  $ $MERLIN server occurrences -identifier-at 4:6 \
  > -log-file log -log-section index \
  > -filename test.ml <test.ml >/dev/null

  $ cat log | grep index_cache -A1 | tail -n 1
  No valid cache found, reindexing.

  $ $MERLIN server occurrences -identifier-at 4:6 \
  > -log-file log -log-section index \
  > -filename test.ml <test.ml >/dev/null

  $ cat log | grep index_cache -A1 | tail -n 1
  Reusing cached value for path $TESTCASE_ROOT/test.ml and stamp 278.

  $ cat >>test.ml <<'EOF'
  > let z = y + y
  > EOF

  $ $MERLIN server occurrences -identifier-at 4:6 \
  > -log-file log -log-section index \
  > -filename test.ml <test.ml >/dev/null

  $ cat log | grep index_cache -A1 | tail -n 1
  No valid cache found, reindexing.

  $ $MERLIN server stop-server
