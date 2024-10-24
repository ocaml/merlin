  $ cat >main.ml <<EOF
  > let f x = succ x
  > EOF

  $ $MERLIN single search-by-polarity -filename ./main.ml \
  > -position 5:25 -query "-ezfnifzen +ezfzef" 
  {
    "class": "exception",
    "value": "Not_found
  Raised at Ocaml_typing__Env.may_lookup_error in file \"src/ocaml/typing/env.ml\", line 2818, characters 7-22
  Called from Ocaml_typing__Env.lookup_type in file \"src/ocaml/typing/env.ml\" (inlined), line 3241, characters 20-62
  Called from Ocaml_typing__Env.find_type_by_name in file \"src/ocaml/typing/env.ml\", line 3326, characters 2-51
  Called from Merlin_analysis__Polarity_search.build_query.prepare in file \"src/analysis/polarity_search.ml\", line 75, characters 19-46
  Called from Merlin_utils__Std.List.filter_map in file \"src/utils/std.ml\", line 108, characters 12-15
  Called from Merlin_analysis__Polarity_search.build_query in file \"src/analysis/polarity_search.ml\", line 80, characters 17-62
  Called from Query_commands.dispatch in file \"src/frontend/query_commands.ml\", line 447, characters 16-55
  Called from Merlin_commands__New_commands.run in file \"src/commands/new_commands.ml\", line 98, characters 15-53
  Called from Merlin_utils__Std.let_ref in file \"src/utils/std.ml\", line 727, characters 8-12
  Re-raised at Merlin_utils__Std.let_ref in file \"src/utils/std.ml\", line 733, characters 4-13
  Called from Merlin_utils__Misc.try_finally in file \"src/utils/misc.ml\", line 45, characters 8-15
  Re-raised at Merlin_utils__Misc.try_finally in file \"src/utils/misc.ml\", line 62, characters 10-24
  Called from Stdlib__Fun.protect in file \"fun.ml\", line 34, characters 8-15
  Re-raised at Stdlib__Fun.protect in file \"fun.ml\", line 39, characters 6-52
  Called from Merlin_kernel__Mocaml.with_state in file \"src/kernel/mocaml.ml\", line 18, characters 8-38
  Re-raised at Merlin_kernel__Mocaml.with_state in file \"src/kernel/mocaml.ml\", line 24, characters 4-15
  Called from Dune__exe__New_merlin.run.(fun) in file \"src/frontend/ocamlmerlin/new/new_merlin.ml\", lines 118-119, characters 16-52
  ",
    "notifications": []
  }
