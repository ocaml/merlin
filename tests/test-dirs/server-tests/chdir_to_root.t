In case server is running, stop it.

  $ $MERLIN server stop-server

Check that the working directory of the server process is correctly restored.

  $ touch test.ml
  $ export MERLIN_LOG=$(pwd)/log

  $ $MERLIN server errors -filename test.ml < test.ml 1>/dev/null
  $ cat log | grep 'old wd'
  changed directory to "$TESTCASE_ROOT" (old wd: "/")
  $ rm log

  $ $MERLIN server errors -filename test.ml < test.ml  1>/dev/null
  $ cat log | grep 'old wd'
  changed directory to "$TESTCASE_ROOT" (old wd: "/")

  $ $MERLIN server stop-server
