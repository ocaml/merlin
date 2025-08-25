The server might already be running, we kill it to make sure we start from a
clean slate:

  $ $MERLIN server stop-server


add two space at the begining of the next line in order to perform the bench
$ time $MERLIN server construct -position 3:21 -filename ctxt.ml < ctxt.ml


Some cleanup.

  $ $MERLIN server stop-server
