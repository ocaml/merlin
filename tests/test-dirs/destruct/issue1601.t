  $ cat >main.ml <<EOF
  > let foo : [< \`Foo ] option = None
  > 
  > let () =
  >   match foo with
  >   | None | Some _ -> ()
  > EOF

  $ $MERLIN single case-analysis -start 5:16 -end 5:16 \
  > -filename main.ml <main.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 5,
          "col": 16
        },
        "end": {
          "line": 5,
          "col": 17
        }
      },
      "`Foo"
    ],
    "notifications": []
  }
  $ $MERLIN single case-analysis -start 5:16 -end 5:16 -filename main.ml <main.ml
  {
    "class": "exception",
    "value": "File \"src/analysis/destruct.ml\", line 558, characters 20-26: Assertion failed
  Raised at Merlin_analysis__Destruct.node in file \"src/analysis/destruct.ml\", line 558, characters 20-32
  Called from Merlin_utils__Misc.try_finally in file \"src/utils/misc.ml\", line 45, characters 8-15
  Re-raised at Merlin_utils__Misc.try_finally in file \"src/utils/misc.ml\", line 62, characters 10-24
  Called from Merlin_utils__Misc.protect_refs.(fun) in file \"src/utils/misc.ml\", line 82, characters 10-14
  Re-raised at Merlin_utils__Misc.protect_refs.(fun) in file \"src/utils/misc.ml\", line 84, characters 38-45
  Called from Ocaml_typing__Persistent_env.without_cmis in file \"src/ocaml/typing/persistent_env.ml\", line 151, characters 10-109
  Called from Merlin_utils__Std.let_ref in file \"src/utils/std.ml\", line 690, characters 8-12
  Re-raised at Merlin_utils__Std.let_ref in file \"src/utils/std.ml\", line 692, characters 30-39
  Called from Dune__exe__New_commands.run in file \"src/frontend/ocamlmerlin/new/new_commands.ml\", line 65, characters 15-53
  Called from Merlin_utils__Std.let_ref in file \"src/utils/std.ml\", line 690, characters 8-12
  Re-raised at Merlin_utils__Std.let_ref in file \"src/utils/std.ml\", line 692, characters 30-39
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
