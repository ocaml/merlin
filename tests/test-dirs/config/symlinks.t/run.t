  $ export ROOT_DIR=$(pwd)

We work in a directory which is a symlink to another
  $ ln -s real link

We need to set the MERLIN_LOG env variable for Merlin to log events prior
to the reading of the configuration.

Merlin should first try a path relative to where `dune ocaml-merlin` was
started. We cannot test the fact that Merlin will retry with an absolute path
because it depends on Dune's version. If Dune is too old and doesn't have the
`dune ocaml-merlin` subcommand no retry is done.
  $ cd link && export MERLIN_LOG=- && ../$MERLIN single dump-configuration -filename main.ml < main.ml \
  >  -log-section Mconfig_log -log-file - 2>&1 | \
  > grep "Querying dune for file: main.ml"
  Querying dune for file: main.ml

However editors will use absolute path to the file which may (or may not ?)
include the symlinks:
  $ cd link && export MERLIN_LOG=- && ../$MERLIN single dump-configuration -filename $(pwd)/../real/main.ml < main.ml \
  >  -log-section Mconfig_log -log-file - 2>&1 | \
  > grep "Querying dune for file: main.ml"
  Querying dune for file: main.ml

  $ cd link && export MERLIN_LOG=- && ../$MERLIN single dump-configuration -filename $(pwd)/main.ml < main.ml \
  >  -log-section Mconfig_log -log-file - 2>&1 | \
  > grep "Querying dune for file: main.ml"
  Querying dune for file: main.ml

And we perform the same testing when Merlin is started from the "real" dir
  $ cd real && export MERLIN_LOG=- && ../$MERLIN single dump-configuration -filename main.ml < main.ml \
  >  -log-section Mconfig_log -log-file - 2>&1 | \
  > grep "Querying dune for file: main.ml"
  Querying dune for file: main.ml

  $ cd real && export MERLIN_LOG=- && ../$MERLIN single dump-configuration -filename $(pwd)/main.ml < main.ml \
  >  -log-section Mconfig_log -log-file - 2>&1 | \
  > grep "Querying dune for file: main.ml"
  Querying dune for file: main.ml

  $ cd real && export MERLIN_LOG=- && ../$MERLIN single dump-configuration -filename $(pwd)/../link/main.ml < main.ml \
  >  -log-section Mconfig_log -log-file - 2>&1 | \
  > grep "Querying dune for file: main.ml"
  Querying dune for file: main.ml
