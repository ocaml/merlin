Test that the typer cache is invalidated when a previously missing cmi is added to a
directory in the build path.

Stop the server incase it's already running.
  $ $MERLIN server stop-server

We create lib/foo.cmi, but lib/bar.cmi is missing.
  $ $OCAMLC -c lib/foo.ml

Prime two typechecker states while lib/bar.cmi is not yet on disk. Both report
an "Unbound module" error.
  $ $MERLIN server errors -filename test.ml < test.ml | jq .value[].message -r
  Unbound module Bar
  $ $MERLIN server errors -filename other.ml < other.ml | jq .value[].message -r
  Unbound module Bar

Now create lib/bar.cmi.
  $ $OCAMLC -c lib/bar.ml

Bump the directory's mtime to defeat File_id's 1-second mtime granularity (the
.cmi above and the test could otherwise share the same second). This only matters on
systems where the mtime granularity is 1 second.
  $ if [ "$(uname)" = "Darwin" ]; then touch -A 02 lib; else touch -d '+2 seconds' lib; fi

The first typechecker state notices that lib changed and refreshes the shared
directory-content cache.
  $ $MERLIN server errors -filename test.ml < test.ml | jq .value[].message -r

The second typechecker state must also notice lib/bar.cmi. In particular, the
shared directory-content cache must not make its stale local load-path snapshot
look current.
  $ $MERLIN server errors -filename other.ml < other.ml | jq .value[].message -r

For reference, single mode (which spawns a fresh process and so cannot reuse
any cache) returns the correct answer:

  $ $MERLIN single errors -filename test.ml < test.ml | jq .value[].message -r

Cleanup.
  $ $MERLIN server stop-server
