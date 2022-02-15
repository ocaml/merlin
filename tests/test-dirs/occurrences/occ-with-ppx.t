  $ cat >dune-project <<EOF
  > (lang dune 2.9)
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name annot)
  >  (libraries yojson ppx_deriving_yojson.runtime)
  >  (preprocess (pps ppx_deriving_yojson)))
  > EOF

  $ cat >annot.ml <<EOF
  > type foo = {
  >   a: int;
  >   b: string;
  > }
  > EOF

  $ dune build @check

  $ $MERLIN single occurrences -identifier-at 1:7 -filename annot.ml <annot.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 5
        },
        "end": {
          "line": 1,
          "col": 8
        }
      }
    ],
    "notifications": []
  }

  $ cat >annot.ml <<EOF
  > type foo = {
  >   a: int;
  >   b: string;
  > }
  > [@@deriving yojson]
  > EOF

  $ dune build @check

FIXME: we expect the same result at before, not the whole type declaration
  $ $MERLIN single occurrences -identifier-at 1:7 -filename annot.ml <annot.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 5,
          "col": 19
        }
      },
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 5,
          "col": 19
        }
      }
    ],
    "notifications": []
  }
