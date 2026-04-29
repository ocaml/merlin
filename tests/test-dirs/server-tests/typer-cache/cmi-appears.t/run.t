Test that the typer cache is invalidated when a previously missing cmi is added to a
directory in the build path.

Stop the server incase it's already running.
  $ $MERLIN server stop-server

We create lib/foo.cmi, but lib/bar.cmi is missing.
  $ $OCAMLC -c lib/foo.ml

First query: lib/bar.cmi is not yet on disk, so we get an "Unbound module" error.
  $ $MERLIN server errors -filename test.ml < test.ml | jq .value[].message -r
  Unbound module Bar

Now create lib/bar.cmi.
  $ $OCAMLC -c lib/bar.ml

Bump the directory's mtime to defeat File_id's 1-second mtime granularity (the
.cmi above and the test could otherwise share the same second). This only matters on
systems where the mtime granularity is 1 second.
  $ if [ "$(uname)" = "Darwin" ]; then touch -A 02 lib; else touch -d '+2 seconds' lib; fi

Second query: lib/bar.cmi is now present, so we shouldn't get an "Unbound module Bar"
error.
  $ $MERLIN server errors -filename test.ml < test.ml | jq .value[].message -r

For reference, single mode (which spawns a fresh process and so cannot reuse
any cache) returns the correct answer:

  $ $MERLIN single errors -filename test.ml < test.ml | jq .value[].message -r

Cleanup.
  $ $MERLIN server stop-server
