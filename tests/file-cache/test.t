The server might already be running, we kill it to make sure we start from a
clean slate:

  $ (killall ocamlmerlin_server.exe; true) &> /dev/null

First try: nothing has been built:

  $ $MERLIN server errors -filename test.ml < test.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 8
        },
        "end": {
          "line": 1,
          "col": 22
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound module Dep"
      }
    ],
    "notifications": []
  }

Then, we build the dep:

  $ $OCAMLC -c dep.ml

And try again:

  $ $MERLIN server errors -filename test.ml < test.ml
  {
    "class": "return",
    "value": [],
    "notifications": []
  }

However if we remove the dep and try again, we get an I/O error:

  $ rm dep.cm*
  $ $MERLIN server errors -filename test.ml -log-file log \
  > -log-section "File_cache(Cmi_cache)" \
  > -log-section "File_cache(Exists_in_directory)" < test.ml
  {
    "class": "error",
    "value": "I/O error: tests/file-cache/dep.cmi: No such file or directory",
    "notifications": []
  }

And the log indicates that we are calling Cmi_cache.read without having gone
through the "Exists_in_directory" cache:

(please excuse the log curating...)

  $ cat log | \
  > sed -E 's:".*(tests/.*):"\1:' | \
  > sed -E 's:".*opam2?/[^/]*/(.*):"~opam/\1:' | \
  > sed -E 's/# [0-9]*\.[0-9]*/#/'
  # File_cache(Cmi_cache) - read
  "tests/file-cache/dep.cmi" was updated on disk
  # File_cache(Cmi_cache) - read
  reading "tests/file-cache/dep.cmi" from disk
  # File_cache(Cmi_cache) - read
  failed to read "tests/file-cache/dep.cmi: No such file or directory"))
