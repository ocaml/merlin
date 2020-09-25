(enabled_if (>= %{ocaml_version} 4.11.0))

The server might already be running, we kill it to make sure we start from a
clean slate:

  $ $MERLIN server stop-server

Then we can look at identifier stamps and whether they are being reset between
buffers, and different runs for the same buffer:

  $ echo "let f x = x" | \
  > $MERLIN server dump -what browse -filename test.ml | \
  > sed 's:\\n:\n:g' | grep Tpat_var
    Tpat_var \"f/78\"
    Tpat_var \"x/80\"

  $ echo "let f x = let () = () in x" | \
  > $MERLIN server dump -what browse -filename test.ml | \
  > sed 's:\\n:\n:g' | grep Tpat_var
    Tpat_var \"f/81\"
    Tpat_var \"x/83\"

  $ echo "let f x = x" | \
  > $MERLIN server dump -what browse -filename other_test.ml | \
  > sed 's:\\n:\n:g' | grep Tpat_var
    Tpat_var \"f/78\"
    Tpat_var \"x/80\"

  $ echo "let f x = let () = () in x" | \
  > $MERLIN server dump -what browse -filename test.ml | \
  > sed 's:\\n:\n:g' | grep Tpat_var
    Tpat_var \"f/78\"
    Tpat_var \"x/80\"

  $ echo "let f x = x" | \
  > $MERLIN server dump -what browse -filename test.ml | \
  > sed 's:\\n:\n:g' | grep Tpat_var
    Tpat_var \"f/81\"
    Tpat_var \"x/83\"

