Reproduction case:

  $ $MERLIN single occurrences -identifier-at 2:14 -filename ./issue827.ml < ./issue827.ml
  {
    "class": "exception",
    "value": "Not_found
  Raised at file \"src/ocaml/typing/407/env.ml\", line 888, characters 13-28
  Called from file \"src/ocaml/typing/407/env.ml\", line 911, characters 33-58
  Called from file \"src/ocaml/typing/407/env.ml\", line 979, characters 6-28
  Called from file \"src/analysis/browse_tree.ml\", line 65, characters 28-52
  Called from file \"src/analysis/browse_tree.ml\", line 83, characters 17-57
  Called from file \"src/analysis/browse_tree.ml\", line 93, characters 12-23
  Called from file \"src/analysis/browse_tree.ml\", line 114, characters 21-59
  Called from file \"list.ml\", line 117, characters 24-34
  Called from file \"list.ml\", line 117, characters 24-34
  Called from file \"list.ml\", line 117, characters 24-34
  Called from file \"src/frontend/query_commands.ml\", line 741, characters 15-68
  Called from file \"src/frontend/query_commands.ml\", line 747, characters 18-55
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

  $ $MERLIN single occurrences -identifier-at 2:19 -filename ./issue827.ml < ./issue827.ml
  {
    "class": "exception",
    "value": "Not_found
  Raised at file \"src/ocaml/typing/407/env.ml\", line 888, characters 13-28
  Called from file \"src/ocaml/typing/407/env.ml\", line 911, characters 33-58
  Called from file \"src/ocaml/typing/407/env.ml\", line 979, characters 6-28
  Called from file \"src/analysis/browse_tree.ml\", line 65, characters 28-52
  Called from file \"src/analysis/browse_tree.ml\", line 83, characters 17-57
  Called from file \"src/analysis/browse_tree.ml\", line 93, characters 12-23
  Called from file \"src/analysis/browse_tree.ml\", line 114, characters 21-59
  Called from file \"list.ml\", line 117, characters 24-34
  Called from file \"list.ml\", line 117, characters 24-34
  Called from file \"list.ml\", line 117, characters 24-34
  Called from file \"src/frontend/query_commands.ml\", line 741, characters 15-68
  Called from file \"src/frontend/query_commands.ml\", line 747, characters 18-55
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

Interestingly if you start from a use instead of the definition, it seems to
work:

  $ $MERLIN single occurrences -identifier-at 4:12 -filename ./issue827.ml < ./issue827.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 13
        },
        "end": {
          "line": 2,
          "col": 15
        }
      },
      {
        "start": {
          "line": 4,
          "col": 8
        },
        "end": {
          "line": 4,
          "col": 12
        }
      }
    ],
    "notifications": []
  }

  $ $MERLIN single occurrences -identifier-at 5:23 -filename ./issue827.ml < ./issue827.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 18
        },
        "end": {
          "line": 2,
          "col": 20
        }
      },
      {
        "start": {
          "line": 5,
          "col": 22
        },
        "end": {
          "line": 5,
          "col": 24
        }
      }
    ],
    "notifications": []
  }
