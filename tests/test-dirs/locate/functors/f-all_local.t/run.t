Check that we can jump locally inside the functor:

  $ $MERLIN single locate -look-for ml -position 14:12 -filename ./all_local.ml < ./all_local.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/all_local.ml",
      "pos": {
        "line": 12,
        "col": 2
      }
    },
    "notifications": []
  }

Check that we can jump from inside the functor to the (sig of the) parameter:

FIXME ? Should we fallback in that manner ? Or go to the Arg ?
# 0.01 locate - reconstructed identifier
t
# 0.01 locate - from_string
inferred context: type
# 0.01 locate - from_string
looking for the source of 't' (prioritizing .ml files)
# 0.01 locate - lookup
lookup in type namespace
# 0.01 locate - shape_of_path
initial:@ Arg/278<All_local.5> . "t"[type]
# 0.01 locate - shape_of_path
reduced:@ Arg/278<All_local.5> . "t"[type]
# 0.01 locate - locate
No UID found in the shape, fallback to lookup location.
# 0.01 locate - result
found: $TESTCASE_ROOT/all_local.ml

  $ $MERLIN single locate -look-for ml -position 12:11 \
  > -filename ./all_local.ml < ./all_local.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/all_local.ml",
      "pos": {
        "line": 2,
        "col": 2
      }
    },
    "notifications": []
  }

Check the argument is substituted for the parameter

  $ $MERLIN single locate -look-for ml -position 20:13 -filename ./all_local.ml < ./all_local.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/all_local.ml",
      "pos": {
        "line": 6,
        "col": 2
      }
    },
    "notifications": []
  }
