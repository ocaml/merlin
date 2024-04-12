This test reproduces issue #1748

  $ cat bar.ml
  module Import = struct
    type hello = string
  end

  $ cat import.ml
  include Bar

  $ cat foo.mli
  open! Import

  $ cat foo.ml
  open! Import

  $ $OCAMLC -c -bin-annot bar.ml import.ml foo.mli foo.ml

Merlin correctly jump to the Import module. Not the one in Bar.
  $ $MERLIN single locate -position 1:10 -look-for implementation \
  > -filename foo.ml < foo.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/import.ml",
    "pos": {
      "line": 1,
      "col": 0
    }
  }

FIXME: It is incorrect to jump to the `Bar.Import` module here:
  $ $MERLIN single locate -position 1:10 -look-for implementation \
  > -filename foo.mli < foo.mli | jq '.value'
  {
    "file": "$TESTCASE_ROOT/bar.ml",
    "pos": {
      "line": 1,
      "col": 0
    }
  }
