Test case-analysis on a function parameter:

  $ cat >fun.ml <<EOF
  > let f x (bb : bool) y = something
  > EOF

FIXME UPGRADE 5.2: this was working before the upgrade
  $ $MERLIN single case-analysis -start 1:10 -end 1:11 \
  > -filename fun.ml <fun.ml | \
  > sed -e 's/, /,/g' | sed -e 's/ *| */|/g' | tr -d '\n' | jq '.'
  {
    "class": "exception",
    "value": "File \"src/analysis/destruct.ml\",line 265,characters 51-57: Assertion failedRaised at Merlin_analysis__Destruct.get_every_pattern.(fun) in file \"src/analysis/destruct.ml\",line 265,characters 51-63Called from Merlin_specific__Browse_raw.(**) in file \"src/ocaml/merlin_specific/browse_raw.ml\" (inlined),line 233,characters 11-25Called from Merlin_specific__Browse_raw.of_pattern in file \"src/ocaml/merlin_specific/browse_raw.ml\",line 274,characters 2-55Called from Merlin_specific__Browse_raw.list_fold in file \"src/ocaml/merlin_specific/browse_raw.ml\",line 236,characters 37-53Called from Merlin_specific__Browse_raw.(**) in file \"src/ocaml/merlin_specific/browse_raw.ml\" (inlined),line 233,characters 11-25Called from Merlin_specific__Browse_raw.of_expression_desc in file \"src/ocaml/merlin_specific/browse_raw.ml\",line 328,characters 4-63Called from Merlin_analysis__Destruct.get_every_pattern in file \"src/analysis/destruct.ml\",lines 263-289,characters 8-31Called from Merlin_analysis__Destruct.node in file \"src/analysis/destruct.ml\",line 591,characters 40-65Called from Merlin_utils__Misc.try_finally in file \"src/utils/misc.ml\",line 45,characters 8-15Re-raised at Merlin_utils__Misc.try_finally in file \"src/utils/misc.ml\",line 62,characters 10-24Called from Merlin_utils__Misc.protect_refs.(fun) in file \"src/utils/misc.ml\",line 82,characters 10-14Re-raised at Merlin_utils__Misc.protect_refs.(fun) in file \"src/utils/misc.ml\",line 84,characters 38-45Called from Ocaml_typing__Persistent_env.without_cmis in file \"src/ocaml/typing/persistent_env.ml\",lines 156-158,characters 10-27Called from Merlin_utils__Std.let_ref in file \"src/utils/std.ml\",line 695,characters 8-12Re-raised at Merlin_utils__Std.let_ref in file \"src/utils/std.ml\",line 697,characters 30-39Called from Dune__exe__New_commands.run in file \"src/frontend/ocamlmerlin/new/new_commands.ml\",line 65,characters 15-53Called from Merlin_utils__Std.let_ref in file \"src/utils/std.ml\",line 695,characters 8-12Re-raised at Merlin_utils__Std.let_ref in file \"src/utils/std.ml\",line 697,characters 30-39Called from Merlin_utils__Misc.try_finally in file \"src/utils/misc.ml\",line 45,characters 8-15Re-raised at Merlin_utils__Misc.try_finally in file \"src/utils/misc.ml\",line 62,characters 10-24Called from Stdlib__Fun.protect in file \"fun.ml\",line 34,characters 8-15Re-raised at Stdlib__Fun.protect in file \"fun.ml\",line 39,characters 6-52Called from Merlin_kernel__Mocaml.with_state in file \"src/kernel/mocaml.ml\",line 18,characters 8-38Re-raised at Merlin_kernel__Mocaml.with_state in file \"src/kernel/mocaml.ml\",line 20,characters 42-53Called from Dune__exe__New_merlin.run.(fun) in file \"src/frontend/ocamlmerlin/new/new_merlin.ml\",lines 103-104,characters 14-50",
    "notifications": []
  }

  $ cat >fun.ml <<EOF
  > let _ = match true with _ as bb -> bb
  > EOF

  $ $MERLIN single case-analysis -start 1:24 -end 1:25 \
  > -filename fun.ml <fun.ml | \
  > sed -e 's/, /,/g' | sed -e 's/ *| */|/g' | tr -d '\n' | jq '.'
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 24
        },
        "end": {
          "line": 1,
          "col": 31
        }
      },
      "(false as bb)|(true as bb)"
    ],
    "notifications": []
  }

  $ cat >fun.ml <<EOF
  > let f x ((false as bb) : bool) y = something
  > EOF

FIXME UPGRADE 5.2: this was working before the upgrade
  $ $MERLIN single case-analysis -start 1:10 -end 1:15 \
  > -filename fun.ml <fun.ml | \
  > sed -e 's/, /,/g' | sed -e 's/ *| */|/g' | tr -d '\n' | jq '.'
  {
    "class": "exception",
    "value": "File \"src/analysis/destruct.ml\",line 265,characters 51-57: Assertion failedRaised at Merlin_analysis__Destruct.get_every_pattern.(fun) in file \"src/analysis/destruct.ml\",line 265,characters 51-63Called from Merlin_specific__Browse_raw.(**) in file \"src/ocaml/merlin_specific/browse_raw.ml\" (inlined),line 233,characters 11-25Called from Merlin_specific__Browse_raw.of_pattern in file \"src/ocaml/merlin_specific/browse_raw.ml\",line 274,characters 2-55Called from Merlin_specific__Browse_raw.list_fold in file \"src/ocaml/merlin_specific/browse_raw.ml\",line 236,characters 37-53Called from Merlin_specific__Browse_raw.(**) in file \"src/ocaml/merlin_specific/browse_raw.ml\" (inlined),line 233,characters 11-25Called from Merlin_specific__Browse_raw.of_expression_desc in file \"src/ocaml/merlin_specific/browse_raw.ml\",line 328,characters 4-63Called from Merlin_analysis__Destruct.get_every_pattern in file \"src/analysis/destruct.ml\",lines 263-289,characters 8-31Called from Merlin_analysis__Destruct.node in file \"src/analysis/destruct.ml\",line 591,characters 40-65Called from Merlin_utils__Misc.try_finally in file \"src/utils/misc.ml\",line 45,characters 8-15Re-raised at Merlin_utils__Misc.try_finally in file \"src/utils/misc.ml\",line 62,characters 10-24Called from Merlin_utils__Misc.protect_refs.(fun) in file \"src/utils/misc.ml\",line 82,characters 10-14Re-raised at Merlin_utils__Misc.protect_refs.(fun) in file \"src/utils/misc.ml\",line 84,characters 38-45Called from Ocaml_typing__Persistent_env.without_cmis in file \"src/ocaml/typing/persistent_env.ml\",lines 156-158,characters 10-27Called from Merlin_utils__Std.let_ref in file \"src/utils/std.ml\",line 695,characters 8-12Re-raised at Merlin_utils__Std.let_ref in file \"src/utils/std.ml\",line 697,characters 30-39Called from Dune__exe__New_commands.run in file \"src/frontend/ocamlmerlin/new/new_commands.ml\",line 65,characters 15-53Called from Merlin_utils__Std.let_ref in file \"src/utils/std.ml\",line 695,characters 8-12Re-raised at Merlin_utils__Std.let_ref in file \"src/utils/std.ml\",line 697,characters 30-39Called from Merlin_utils__Misc.try_finally in file \"src/utils/misc.ml\",line 45,characters 8-15Re-raised at Merlin_utils__Misc.try_finally in file \"src/utils/misc.ml\",line 62,characters 10-24Called from Stdlib__Fun.protect in file \"fun.ml\",line 34,characters 8-15Re-raised at Stdlib__Fun.protect in file \"fun.ml\",line 39,characters 6-52Called from Merlin_kernel__Mocaml.with_state in file \"src/kernel/mocaml.ml\",line 18,characters 8-38Re-raised at Merlin_kernel__Mocaml.with_state in file \"src/kernel/mocaml.ml\",line 20,characters 42-53Called from Dune__exe__New_merlin.run.(fun) in file \"src/frontend/ocamlmerlin/new/new_merlin.ml\",lines 103-104,characters 14-50",
    "notifications": []
  }
