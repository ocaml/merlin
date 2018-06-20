TODO: test all error cases

  $ $MERLIN single case-analysis -start 4:9 -end 4:11 -filename nothing_to_do.ml <<EOF \
  > let _ = \
  >   match (None : unit option) with \
  >   | None -> () \
  >   | Some () -> () \
  > EOF
  {
    "class": "error",
    "value": "Error: Nothing to do",
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
