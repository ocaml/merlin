  $ cat >main.mli <<'EOF'
  > type t = unit
  > val x : t
  > EOF

  $ cat >main.ml <<'EOF'
  > let x = ()
  > type t = unit
  > let _ : t = ()
  > EOF

  $ ocamlc -bin-annot -bin-annot-occurrences -c main.mli main.ml

  $ ocaml-index aggregate main.cmti main.cmt

The indexer should not mixup uids from mli and ml files:
  $ ocaml-index dump project.ocaml-index
  4 uids:
  {uid: [intf]Main.0; locs:
     "t": File "main.mli", line 1, characters 5-6;
     "t": File "main.mli", line 2, characters 8-9
   uid: Main.0; locs: "x": File "main.ml", line 1, characters 4-5
   uid: [intf]Main.1; locs: "x": File "main.mli", line 2, characters 4-5
   uid: Main.1; locs:
     "t": File "main.ml", line 2, characters 5-6;
     "t": File "main.ml", line 3, characters 8-9
   }, 0 approx shapes: {}, and shapes for CUS .
  and related uids:{(Main.0 [intf]Main.1); ([intf]Main.0 Main.1)}

Merlin should not mixup uids from mli and ml files, and return results in both
the interface and the implementation.
  $ $MERLIN single occurrences -scope project -identifier-at 2:8 \
  > -index-file project.ocaml-index \
  > -filename main.mli <main.mli
  {
    "class": "return",
    "value": [
      {
        "file": "$TESTCASE_ROOT/main.mli",
        "start": {
          "line": 1,
          "col": 5
        },
        "end": {
          "line": 1,
          "col": 6
        }
      },
      {
        "file": "$TESTCASE_ROOT/main.ml",
        "start": {
          "line": 2,
          "col": 5
        },
        "end": {
          "line": 2,
          "col": 6
        }
      },
      {
        "file": "$TESTCASE_ROOT/main.ml",
        "start": {
          "line": 3,
          "col": 8
        },
        "end": {
          "line": 3,
          "col": 9
        }
      },
      {
        "file": "$TESTCASE_ROOT/main.mli",
        "start": {
          "line": 2,
          "col": 8
        },
        "end": {
          "line": 2,
          "col": 9
        }
      }
    ],
    "notifications": []
  }

Same when the cursor is at the origin:
  $ $MERLIN single occurrences -scope project -identifier-at 1:5 \
  > -index-file project.ocaml-index  \
  > -filename main.mli <main.mli
  {
    "class": "return",
    "value": [
      {
        "file": "$TESTCASE_ROOT/main.mli",
        "start": {
          "line": 1,
          "col": 5
        },
        "end": {
          "line": 1,
          "col": 6
        }
      },
      {
        "file": "$TESTCASE_ROOT/main.ml",
        "start": {
          "line": 2,
          "col": 5
        },
        "end": {
          "line": 2,
          "col": 6
        }
      },
      {
        "file": "$TESTCASE_ROOT/main.ml",
        "start": {
          "line": 3,
          "col": 8
        },
        "end": {
          "line": 3,
          "col": 9
        }
      },
      {
        "file": "$TESTCASE_ROOT/main.mli",
        "start": {
          "line": 2,
          "col": 8
        },
        "end": {
          "line": 2,
          "col": 9
        }
      }
    ],
    "notifications": []
  }

It also works when querying for t from the implementation:
  $ $MERLIN single occurrences -scope project -identifier-at 3:8 \
  > -index-file project.ocaml-index \
  > -filename main.ml <main.ml
  {
    "class": "return",
    "value": [
      {
        "file": "$TESTCASE_ROOT/main.ml",
        "start": {
          "line": 2,
          "col": 5
        },
        "end": {
          "line": 2,
          "col": 6
        }
      },
      {
        "file": "$TESTCASE_ROOT/main.ml",
        "start": {
          "line": 3,
          "col": 8
        },
        "end": {
          "line": 3,
          "col": 9
        }
      },
      {
        "file": "$TESTCASE_ROOT/main.mli",
        "start": {
          "line": 1,
          "col": 5
        },
        "end": {
          "line": 1,
          "col": 6
        }
      },
      {
        "file": "$TESTCASE_ROOT/main.mli",
        "start": {
          "line": 2,
          "col": 8
        },
        "end": {
          "line": 2,
          "col": 9
        }
      }
    ],
    "notifications": []
  }


It also works when querying for x from the implementation:
  $ $MERLIN single occurrences -scope project -identifier-at 1:4 \
  > -index-file project.ocaml-index \
  > -filename main.ml <main.ml
  {
    "class": "return",
    "value": [
      {
        "file": "$TESTCASE_ROOT/main.ml",
        "start": {
          "line": 1,
          "col": 4
        },
        "end": {
          "line": 1,
          "col": 5
        }
      },
      {
        "file": "$TESTCASE_ROOT/main.mli",
        "start": {
          "line": 2,
          "col": 4
        },
        "end": {
          "line": 2,
          "col": 5
        }
      }
    ],
    "notifications": []
  }

It also works when querying for x from the interface:
  $ $MERLIN single occurrences -scope project -identifier-at 2:4 \
  > -index-file project.ocaml-index \
  > -filename main.mli <main.mli
  {
    "class": "return",
    "value": [
      {
        "file": "$TESTCASE_ROOT/main.mli",
        "start": {
          "line": 2,
          "col": 4
        },
        "end": {
          "line": 2,
          "col": 5
        }
      },
      {
        "file": "$TESTCASE_ROOT/main.ml",
        "start": {
          "line": 1,
          "col": 4
        },
        "end": {
          "line": 1,
          "col": 5
        }
      }
    ],
    "notifications": []
  }

If we switch lines without rebuilding...
  $ cat >main.ml <<'EOF'
  > type t = unit
  > let x = ()
  > let _ : t = ()
  > EOF

Merlin should not get confused and return an occurrence of `x` in the interface
when asked from occurrences of `t` in the implementation. 

FIXME: this is based on a heuristic that compares the identifiers it could still
get confused if both identifers are the same.
  $ $MERLIN single occurrences -scope project -identifier-at 1:5 \
  > -index-file project.ocaml-index \
  > -filename main.ml <main.ml
  {
    "class": "return",
    "value": [
      {
        "file": "$TESTCASE_ROOT/main.ml",
        "start": {
          "line": 1,
          "col": 5
        },
        "end": {
          "line": 1,
          "col": 6
        }
      },
      {
        "file": "$TESTCASE_ROOT/main.ml",
        "start": {
          "line": 3,
          "col": 8
        },
        "end": {
          "line": 3,
          "col": 9
        }
      }
    ],
    "notifications": []
  }
