  $ cat >main.ml <<'EOF'
  > let x' = 1
  > let x = 41
  > let f x = x
  > let y = f x
  > EOF

  $ $MERLIN server occurrences -scope local -identifier-at 3:10 \
  > -log-file log_1 -log-section index \
  > -filename main.ml <main.ml >/dev/null 

  $ cat >main.ml <<'EOF'
  > let x' = 1
  > let x = 42
  > let f x = x 
  > let y = f x
  > EOF

  $ $MERLIN server occurrences -scope local -identifier-at 3:10 \
  > -log-file log_2 -log-section index \
  > -filename main.ml <main.ml >/dev/null

The uids should be the same on both queries:
  $ cat log_1 | grep Found | cat >log_1g
  $ cat log_2 | grep Found | cat >log_2g
  $ diff log_1g log_2g

  $ $MERLIN server stop-server
