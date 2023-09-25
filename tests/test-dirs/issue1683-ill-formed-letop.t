  $ cat >main.ml <<EOF
  > let (let+) x f = f x
  > 
  > let _ =
  >   let+ x, _ = 42, 43 in 
  >   x
  > EOF

FIXME: new handling of letops is broken when matching on a tuple
  $ $MERLIN single holes -filename main.ml <main.ml
  {
    "class": "exception",
    "value": "Invalid_argument(\"List.combine\")
  Raised at Stdlib.invalid_arg in file \"stdlib.ml\", line 30, characters 20-45
  Called from Stdlib__List.combine in file \"list.ml\", line 305, characters 36-49
  Called from Merlin_specific__Browse_raw.of_expression_desc in file \"src/ocaml/merlin_specific/browse_raw.ml\", line 392, characters 23-54
  Called from Merlin_specific__Browse_raw.fold_node in file \"src/ocaml/merlin_specific/browse_raw.ml\" (inlined), line 698, characters 2-24
  Called from Merlin_specific__Browse_raw.all_holes.aux in file \"src/ocaml/merlin_specific/browse_raw.ml\", line 987, characters 4-28
  Called from Merlin_specific__Browse_raw.(**) in file \"src/ocaml/merlin_specific/browse_raw.ml\" (inlined), line 233, characters 11-25
  Called from Merlin_specific__Browse_raw.of_expression in file \"src/ocaml/merlin_specific/browse_raw.ml\", line 267, characters 22-78
  Called from Merlin_specific__Browse_raw.list_fold in file \"src/ocaml/merlin_specific/browse_raw.ml\", line 236, characters 37-53
  Called from Merlin_specific__Browse_raw.all_holes in file \"src/ocaml/merlin_specific/browse_raw.ml\", line 989, characters 2-20
  Called from Query_commands.dispatch.loc_and_types_of_holes in file \"src/frontend/query_commands.ml\", line 613, characters 15-42
  Called from Stdlib__List.map in file \"list.ml\", line 92, characters 20-23
  Called from Merlin_utils__Std.List.concat_map in file \"src/utils/std.ml\", line 129, characters 32-42
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

