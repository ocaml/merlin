The server might already be running, we kill it to make sure we start from a
clean slate:

  $ $MERLIN server stop-server


add two space at the begining of the next line in order to perform the bench
$ time $MERLIN server construct -position 3:21 -filename ctxt.ml < ctxt.ml

  $ touch ctxt.ml

add two space at the begining of the next line in order to perform the bench
$ time $MERLIN server complete-prefix -position 109:14 -prefix fo -filename ctxt.ml < ctxt.ml

  $ touch ctxt.ml

add two space at the begining of the next line in order to perform the bench
$ time $MERLIN server complete-prefix -position 51152:12 -prefix xy -filename ctxt.ml < ctxt.ml

  $ touch ctxt.ml

add two space at the begining of the next line in order to perform the bench
$ time $MERLIN server case-analysis -start 50796:25 -end 50796:25 -filename ctxt.ml < ctxt.ml

  $ touch ctxt.ml

add two space at the begining of the next line in order to perform the bench
$ time $MERLIN server case-analysis -start 50796:25 -end 50796:25 -filename ctxt.ml < ctxt.ml

  $ touch ctxt.ml


add two space at the begining of the next line in order to perform the bench
$ time $MERLIN server case-analysis -start 51318:43 -end 51318:43 -filename ctxt.ml < ctxt.ml

  $ touch ctxt.ml

add two space at the begining of the next line in order to perform the bench
$ time $MERLIN server complete-prefix -position 51319:30 -prefix UnregistrationParams.B -filename ctxt.ml < ctxt.ml

  $ touch ctxt.ml

Some cleanup.

  $ $MERLIN server stop-server
