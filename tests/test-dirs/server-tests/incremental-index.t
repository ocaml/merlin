  $ $MERLIN server stop-server

  $ cat >main.ml <<'EOF'
  > let x = ()
  > let _ = x
  > EOF

The entire buffer is indexed:
  $ $MERLIN server occurrences -identifier-at 2:8 \
  > -log-file log -log-section index-occurrences \
  > -filename main.ml <main.ml | jq '.value[].start'
  {
    "line": 1,
    "col": 4
  }
  {
    "line": 2,
    "col": 8
  }

  $ cat log | grep "Found"
  Found () (File "$TESTCASE_ROOT/main.ml", line 1, characters 8-10) wiht uid <predef:()>
  Found x (File "$TESTCASE_ROOT/main.ml", line 2, characters 8-9) wiht uid Main.0

No changes, no indexation:
  $ $MERLIN server occurrences -identifier-at 2:8 \
  > -log-file log -log-section index-occurrences \
  > -filename main.ml <main.ml | jq '.value[].start'
  {
    "line": 1,
    "col": 4
  }
  {
    "line": 2,
    "col": 8
  }

  $ cat log | grep "Found"
  [1]

  $ cat >main.ml <<'EOF'
  > let x = ()
  > let _ = x
  > let _ = x
  > let _ = x
  > EOF

Only the new item is indexed
  $ $MERLIN server occurrences -identifier-at 2:8 \
  > -log-file log -log-section index-occurrences \
  > -filename main.ml <main.ml | jq '.value[].start'
  {
    "line": 1,
    "col": 4
  }
  {
    "line": 2,
    "col": 8
  }
  {
    "line": 3,
    "col": 8
  }
  {
    "line": 4,
    "col": 8
  }

  $ cat log | grep "Found"
  Found x (File "$TESTCASE_ROOT/main.ml", line 3, characters 8-9) wiht uid Main.0
  Found x (File "$TESTCASE_ROOT/main.ml", line 4, characters 8-9) wiht uid Main.0

  $ cat >main.ml <<'EOF'
  > let x = ()
  > let _ = x
  > 
  > let _ = x
  > EOF

Only the line after the removed one are re-indexed
  $ $MERLIN server occurrences -identifier-at 2:8 \
  > -log-file log -log-section index-occurrences \
  > -filename main.ml <main.ml | jq '.value[].start'
  {
    "line": 1,
    "col": 4
  }
  {
    "line": 2,
    "col": 8
  }
  {
    "line": 4,
    "col": 8
  }

  $ cat log | grep "Found"
  Found x (File "$TESTCASE_ROOT/main.ml", line 4, characters 8-9) wiht uid Main.0

The edited line is reindexed with the rest of the buffer
  $ cat >main.ml <<'EOF'
  > let x = ()
  > let _ = x
  > 
  > let _ = x; x
  > EOF

  $ $MERLIN server occurrences -identifier-at 2:8 \
  > -log-file log -log-section index-occurrences \
  > -filename main.ml <main.ml | jq '.value[].start'
  {
    "line": 1,
    "col": 4
  }
  {
    "line": 2,
    "col": 8
  }
  {
    "line": 4,
    "col": 8
  }
  {
    "line": 4,
    "col": 11
  }

  $ cat log | grep "Found"
  Found x (File "$TESTCASE_ROOT/main.ml", line 4, characters 8-9) wiht uid Main.0
  Found x (File "$TESTCASE_ROOT/main.ml", line 4, characters 11-12) wiht uid Main.0

  $ $MERLIN server stop-server
