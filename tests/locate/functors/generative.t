Check that we handle generative functors properly:

  $ $MERLIN single locate -position 13:12 -filename generative.ml < generative.ml
  {
    "class": "exception",
    "value": "File \"src/analysis/typedtrie.ml\", line 633, characters 18-24: Assertion failed
  Raised at file \"src/analysis/typedtrie.ml\", line 633, characters 18-47
  Called from file \"src/analysis/typedtrie.ml\", line 654, characters 8-31
  Called from file \"src/analysis/typedtrie.ml\", line 654, characters 8-31
  Called from file \"src/analysis/locate.ml\", line 465, characters 8-68
  Called from file \"src/analysis/locate.ml\", line 782, characters 10-57
  Called from file \"src/analysis/locate.ml\", line 851, characters 10-70
  Called from file \"src/utils/std.ml\", line 679, characters 8-12
  Re-raised at file \"src/utils/std.ml\", line 681, characters 30-39
  Called from file \"src/frontend/query_commands.ml\", line 513, characters 6-119
  Called from file \"src/utils/local_store.ml\", line 29, characters 8-12
  Re-raised at file \"src/utils/local_store.ml\", line 37, characters 4-15
  Called from file \"src/kernel/mocaml.ml\", line 40, characters 8-38
  Re-raised at file \"src/kernel/mocaml.ml\", line 48, characters 4-15
  Called from file \"src/frontend/new/new_commands.ml\", line 65, characters 15-53
  Called from file \"src/utils/std.ml\", line 679, characters 8-12
  Re-raised at file \"src/utils/std.ml\", line 681, characters 30-39
  Called from file \"src/ocaml/utils/misc.ml\", line 30, characters 8-15
  Re-raised at file \"src/ocaml/utils/misc.ml\", line 42, characters 10-24
  Called from file \"src/frontend/new/new_merlin.ml\", line 99, characters 18-54
  ",
    "notifications": []
  }
