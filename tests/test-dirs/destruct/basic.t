TODO: test all error cases

  $ $MERLIN single case-analysis -start 4:9 -end 4:11 -filename nothing_to_do.ml <<EOF \
  > let _ = \
  >   match (None : unit option) with \
  >   | None -> () \
  >   | Some () -> () \
  > EOF
  {
    "class": "error",
    "value": "Nothing to do",
    "notifications": []
  }


  $ $MERLIN single case-analysis -start 3:4 -end 3:8 -filename make_exhaustive.ml <<EOF \
  > let _ = \
  >   match (None : unit option) with \
  >   | None -> () \
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 14
        },
        "end": {
          "line": 3,
          "col": 14
        }
      },
      "
  | Some _ -> (??)"
    ],
    "notifications": []
  }

  $ $MERLIN single case-analysis -start 4:9 -end 4:10 -filename refine_pattern.ml <<EOF \
  > let _ = \
  >   match (None : unit option) with \
  >   | None -> () \
  >   | Some _ -> () \
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 9
        },
        "end": {
          "line": 4,
          "col": 10
        }
      },
      "()"
    ],
    "notifications": []
  }

  $ $MERLIN single case-analysis -start 4:2 -end 4:3 -filename unpack_module.ml <<EOF \
  > module type S = sig end \
  >  \
  > let g (x : (module S)) = \
  >   x \
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 2
        },
        "end": {
          "line": 4,
          "col": 3
        }
      },
      "let module M = (val x) in (??)"
    ],
    "notifications": []
  }

  $ $MERLIN single case-analysis -start 2:2 -end 2:3 -filename record_exp.ml <<EOF \
  > let f (x : int ref) = \
  >   x \
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 2
        },
        "end": {
          "line": 2,
          "col": 3
        }
      },
      "match x with | { contents } -> (??)"
    ],
    "notifications": []
  }

FIXME: put each case on a different line (if it doesn't require updating
pprintast).

  $ $MERLIN single case-analysis -start 2:2 -end 2:3 -filename variant_exp.ml <<EOF \
  > let f (x : int option) = \
  >   x \
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 2
        },
        "end": {
          "line": 2,
          "col": 3
        }
      },
      "match x with | None -> (??) | Some _ -> (??)"
    ],
    "notifications": []
  }

  $ $MERLIN single case-analysis -start 1:4 -end 1:4 -filename stacktrace.ml <<EOF \
  > let () = () \
  > EOF
  {
    "class": "exception",
    "value": "Invalid_argument(\"get_every_pattern: [{\\\"filename\\\":\\\"stacktrace.ml\\\",\\\"start\\\":{\\\"line\\\":1,\\\"col\\\":0},\\\"end\\\":{\\\"line\\\":1,\\\"col\\\":11},\\\"ghost\\\":false,\\\"attrs\\\":[],\\\"kind\\\":\\\"value_binding\\\",\\\"children\\\":[{\\\"filename\\\":\\\"stacktrace.ml\\\",\\\"start\\\":{\\\"line\\\":1,\\\"col\\\":4},\\\"end\\\":{\\\"line\\\":1,\\\"col\\\":6},\\\"ghost\\\":false,\\\"attrs\\\":[],\\\"kind\\\":\\\"pattern (stacktrace.ml[1,0+4]..stacktrace.ml[1,0+6])\\\
    Tpat_construct \\\\\\\"()\\\\\\\"\\\
    []\\\
  \\\",\\\"children\\\":[]},{\\\"filename\\\":\\\"stacktrace.ml\\\",\\\"start\\\":{\\\"line\\\":1,\\\"col\\\":9},\\\"end\\\":{\\\"line\\\":1,\\\"col\\\":11},\\\"ghost\\\":false,\\\"attrs\\\":[],\\\"kind\\\":\\\"expression\\\",\\\"children\\\":[]}]}]\")
  Raised at file \"stdlib.ml\", line 30, characters 20-45
  Called from file \"src/analysis/destruct.ml\", line 426, characters 34-59
  Called from file \"src/ocaml/utils/misc.ml\", line 62, characters 8-15
  Re-raised at file \"src/ocaml/utils/misc.ml\", line 79, characters 10-24
  Called from file \"src/ocaml/utils/misc.ml\", line 94, characters 10-14
  Re-raised at file \"src/ocaml/utils/misc.ml\", line 96, characters 38-45
  Called from file \"src/ocaml/typing/409/persistent_env.ml\", line 142, characters 10-109
  Called from file \"src/utils/std.ml\", line 679, characters 8-12
  Re-raised at file \"src/utils/std.ml\", line 681, characters 30-39
  Called from file \"src/frontend/ocamlmerlin/new/new_commands.ml\", line 64, characters 15-53
  Called from file \"src/utils/std.ml\", line 679, characters 8-12
  Re-raised at file \"src/utils/std.ml\", line 681, characters 30-39
  Called from file \"src/ocaml/utils/misc.ml\", line 62, characters 8-15
  Re-raised at file \"src/ocaml/utils/misc.ml\", line 79, characters 10-24
  Called from file \"src/utils/local_store.ml\", line 42, characters 8-12
  Re-raised at file \"src/utils/local_store.ml\", line 52, characters 4-15
  Called from file \"src/kernel/mocaml.ml\", line 18, characters 8-38
  Re-raised at file \"src/kernel/mocaml.ml\", line 20, characters 42-53
  Called from file \"src/frontend/ocamlmerlin/new/new_merlin.ml\", line 100, characters 14-110
  ",
    "notifications": []
  }
