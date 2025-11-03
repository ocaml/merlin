The server might already be running, we kill it to make sure we start from a
clean slate:
  $ $MERLIN server stop-server

Then we can look at identifier stamps and whether they are being reset between
buffers, and different runs for the same buffer:

  $ echo "let f x = x" | \
  > $MERLIN server dump -what browse -filename test.ml | \
  > sed 's:\\n:\n:g' | grep Tpat_var
    Tpat_var \"f/274\"
    Tpat_var \"x/276\"

  $ echo "let f x = let () = () in x" | \
  > $MERLIN server dump -what browse -filename test.ml | \
  > sed 's:\\n:\n:g' | grep Tpat_var
    Tpat_var \"f/277\"
    Tpat_var \"x/279\"

  $ echo "let f x = x" | \
  > $MERLIN server dump -what browse -filename other_test.ml | \
  > sed 's:\\n:\n:g' | grep Tpat_var
    Tpat_var \"f/274\"
    Tpat_var \"x/276\"

  $ echo "let f x = let () = () in x" | \
  > $MERLIN server dump -what browse -filename test.ml | \
  > sed 's:\\n:\n:g' | grep Tpat_var
    Tpat_var \"f/277\"
    Tpat_var \"x/279\"

  $ echo "let f x = x" | \
  > $MERLIN server dump -what browse -filename test.ml | \
  > sed 's:\\n:\n:g' | grep Tpat_var
    Tpat_var \"f/280\"
    Tpat_var \"x/282\"

  $ $MERLIN server stop-server
