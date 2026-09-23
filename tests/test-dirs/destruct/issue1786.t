Create the types Box.t and Either.t, each of which are in their own module and file.
Box.t depends on Either.t.

  $ cat >either.ml <<EOF
  > type t = Left | Right
  > EOF

  $ cat >box.ml <<EOF
  > type t = Box of Either.t
  > EOF

  $ $OCAMLC either.ml box.ml

Create the file that we'll use to demonstrate the error

  $ touch foo.ml

In Persistent_env.find_pers_struct, the module Either fails to be found:

  $ cat > foo.ml <<EOF
  > let f (x : Box.t) =
  >   match x with
  >   | Box _ -> 10
  > EOF
  $ LOC=3:8
  $ show_location foo.ml $LOC
  let f (x : Box.t) =
    match x with
    | Box █ -> 10
  $ $MERLIN single case-analysis -start 3:8 -end 3:8 -filename foo.ml < foo.ml
  {
    "class": "exception",
    "value": "Not_found
  Raised at Ocaml_typing__Persistent_env.find_pers_struct in file \"src/ocaml/typing/persistent_env.ml\", line 262, characters 28-43
  Called from Ocaml_typing__Persistent_env.find in file \"src/ocaml/typing/persistent_env.ml\", line 314, characters 6-59
  Called from Ocaml_typing__Env.find_module_components in file \"src/ocaml/typing/env.ml\", line 1128, characters 17-43
  Called from Ocaml_typing__Env.find_structure_components in file \"src/ocaml/typing/env.ml\", line 1139, characters 23-56
  Called from Ocaml_typing__Env.find_type_data in file \"src/ocaml/typing/env.ml\", line 1239, characters 19-50
  Called from Ocaml_typing__Env.find_type_descrs in file \"src/ocaml/typing/env.ml\", line 1315, characters 2-24
  Called from Merlin_analysis__Destruct.gen_patterns in file \"src/analysis/destruct.ml\", line 100, characters 16-45
  Called from Merlin_analysis__Destruct.refine_complete_match in file \"src/analysis/destruct.ml\", line 731, characters 18-46
  Called from Ocaml_utils__Misc.try_finally in file \"src/ocaml/utils/misc.ml\", line 35, characters 8-15
  Re-raised at Ocaml_utils__Misc.try_finally in file \"src/ocaml/utils/misc.ml\", line 52, characters 10-24
  Called from Stdlib__Fun.protect in file \"fun.ml\", line 34, characters 8-15
  Re-raised at Stdlib__Fun.protect in file \"fun.ml\", line 39, characters 6-52
  Called from Ocaml_typing__Persistent_env.without_cmis in file \"src/ocaml/typing/persistent_env.ml\", lines 158-160, characters 10-27
  Called from Merlin_utils__Std.let_ref in file \"src/utils/std.ml\", line 697, characters 8-12
  Re-raised at Merlin_utils__Std.let_ref in file \"src/utils/std.ml\", line 703, characters 4-13
  Called from Merlin_commands__New_commands.run in file \"src/commands/new_commands.ml\", line 98, characters 15-53
  Called from Merlin_utils__Std.let_ref in file \"src/utils/std.ml\", line 697, characters 8-12
  Re-raised at Merlin_utils__Std.let_ref in file \"src/utils/std.ml\", line 703, characters 4-13
  Called from Merlin_utils__Std.let_ref in file \"src/utils/std.ml\", line 697, characters 8-12
  Re-raised at Merlin_utils__Std.let_ref in file \"src/utils/std.ml\", line 703, characters 4-13
  Called from Ocaml_utils__Misc.try_finally in file \"src/ocaml/utils/misc.ml\", line 35, characters 8-15
  Re-raised at Ocaml_utils__Misc.try_finally in file \"src/ocaml/utils/misc.ml\", line 52, characters 10-24
  Called from Stdlib__Fun.protect in file \"fun.ml\", line 34, characters 8-15
  Re-raised at Stdlib__Fun.protect in file \"fun.ml\", line 39, characters 6-52
  Called from Merlin_kernel__Mocaml.with_state in file \"src/kernel/mocaml.ml\", line 18, characters 8-38
  Re-raised at Merlin_kernel__Mocaml.with_state in file \"src/kernel/mocaml.ml\", line 24, characters 4-15
  Called from Dune__exe__New_merlin.run.(fun) in file \"src/frontend/ocamlmerlin/new/new_merlin.ml\", lines 118-119, characters 16-52
  ",
    "notifications": []
  }

If we explicitly use Either (by opening it), we force it to load and the query works
as expected:

  $ $MERLIN single case-analysis -start 4:8 -end 4:8 -filename foo.ml <<EOF
  > open Either
  > let f (x : Box.t) =
  >   match x with
  >   | Box _ -> 10
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 4
        },
        "end": {
          "line": 4,
          "col": 9
        }
      },
      "Box (Left) | Box (Right)"
    ],
    "notifications": []
  }
