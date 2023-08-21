See issue #1185 on vscode-ocaml-platform

  $ cat >main.ml <<EOF
  > (*type t = ..*)
  > type t  = A
  > 
  > let foo (x : t) = match x with
  > | A -> 1
  > | _ -> 0
  > EOF

  $ $MERLIN single occurrences -identifier-at 5:2 \
  > -filename main.ml <main.ml | jq '.value'
  [
    {
      "start": {
        "line": 2,
        "col": 10
      },
      "end": {
        "line": 2,
        "col": 11
      }
    },
    {
      "start": {
        "line": 5,
        "col": 2
      },
      "end": {
        "line": 5,
        "col": 3
      }
    }
  ]


  $ cat >main.ml <<EOF
  > type t = ..
  > type t += A
  > 
  > let foo (x : t) = match x with
  > | A -> 1
  > | _ -> 0
  > EOF

FIXME: we can do better than that
  $ $MERLIN single occurrences -identifier-at 5:2 \
  > -log-file - -log-section occurrences \
  > -filename main.ml <main.ml 
  {
    "class": "exception",
    "value": "File \"src/analysis/browse_tree.ml\", line 88, characters 15-21: Assertion failed
  Raised at Merlin_analysis__Browse_tree.same_constructor.get_decls in file \"src/analysis/browse_tree.ml\", line 88, characters 15-27
  Called from Merlin_analysis__Browse_tree.same_constructor in file \"src/analysis/browse_tree.ml\", line 93, characters 12-23
  Called from Merlin_analysis__Browse_tree.all_constructor_occurrences.aux in file \"src/analysis/browse_tree.ml\", line 117, characters 14-52
  Called from Stdlib__List.fold_left in file \"list.ml\", line 121, characters 24-34
  Called from Stdlib__List.fold_left in file \"list.ml\", line 121, characters 24-34
  Called from Stdlib__List.fold_left in file \"list.ml\", line 121, characters 24-34
  Called from Stdlib__List.fold_left in file \"list.ml\", line 121, characters 24-34
  Called from Stdlib__List.fold_left in file \"list.ml\", line 121, characters 24-34
  Called from Stdlib__List.fold_left in file \"list.ml\", line 121, characters 24-34
  Called from Stdlib__List.fold_left in file \"list.ml\", line 121, characters 24-34
  Called from Stdlib__List.fold_left in file \"list.ml\", line 121, characters 24-34
  Called from Query_commands.dispatch.constructor_occurrence in file \"src/frontend/query_commands.ml\", line 829, characters 15-72
  Called from Dune__exe__New_commands.run in file \"src/frontend/ocamlmerlin/new/new_commands.ml\", line 65, characters 15-53
  Called from Merlin_utils__Std.let_ref in file \"src/utils/std.ml\", line 693, characters 8-12
  Re-raised at Merlin_utils__Std.let_ref in file \"src/utils/std.ml\", line 695, characters 30-39
  Called from Merlin_utils__Misc.try_finally in file \"src/utils/misc.ml\", line 45, characters 8-15
  Re-raised at Merlin_utils__Misc.try_finally in file \"src/utils/misc.ml\", line 62, characters 10-24
  Called from Stdlib__Fun.protect in file \"fun.ml\", line 33, characters 8-15
  Re-raised at Stdlib__Fun.protect in file \"fun.ml\", line 38, characters 6-52
  Called from Merlin_kernel__Mocaml.with_state in file \"src/kernel/mocaml.ml\", line 18, characters 8-38
  Re-raised at Merlin_kernel__Mocaml.with_state in file \"src/kernel/mocaml.ml\", line 20, characters 42-53
  Called from Dune__exe__New_merlin.run.(fun) in file \"src/frontend/ocamlmerlin/new/new_merlin.ml\", line 101, characters 14-110
  ",
    "notifications": []
  }

