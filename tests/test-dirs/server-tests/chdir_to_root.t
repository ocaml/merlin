In case server is running, stop it.

  $ $MERLIN server stop-server

Check that the working directory of the server process is correctly restored.

  $ touch test.ml
  $ export MERLIN_LOG=-
  $ $MERLIN server errors -filename test.ml < test.ml 2>&1 | grep 'old wd'
  changed directory to "$TESTCASE_ROOT" (old wd: "/")
  $ $MERLIN server errors -filename test.ml < test.ml 2>&1 | grep 'old wd'
  changed directory to "$TESTCASE_ROOT" (old wd: "/")
  $ $MERLIN server stop-server
