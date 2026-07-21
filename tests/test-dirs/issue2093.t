  $ cat >test.mli <<EOF
  > val last_2 : 'a list -> ('a * 'a) option
  > EOF
FIXME: the current results promoted is wrong. 
  $ $MERLIN single type-enclosing -filename test.mli -position 1:28 -verbosity 0 -index 0 < test.mli
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 28
        },
        "end": {
          "line": 1,
          "col": 29
        },
        "type": "int -> int -> int",
        "tail": "no"
      },
      {
        "start": {
          "line": 1,
          "col": 25
        },
        "end": {
          "line": 1,
          "col": 32
        },
        "type": 1,
        "tail": "no"
      },
      {
        "start": {
          "line": 1,
          "col": 24
        },
        "end": {
          "line": 1,
          "col": 40
        },
        "type": 2,
        "tail": "no"
      },
      {
        "start": {
          "line": 1,
          "col": 13
        },
        "end": {
          "line": 1,
          "col": 40
        },
        "type": 3,
        "tail": "no"
      },
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 1,
          "col": 40
        },
        "type": 4,
        "tail": "no"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single document -filename test.mli -position 1:28 < test.mli
  {
    "class": "return",
    "value": "Integer multiplication.
      Left-associative operator, see {!Ocaml_operators} for more information.",
    "notifications": []
  }

