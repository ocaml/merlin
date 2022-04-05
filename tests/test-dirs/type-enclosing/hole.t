Check that we can access the expected type of a hole:
  $ $MERLIN single type-enclosing -position 2:2 -filename hole.ml <<EOF
  > let f () : int =
  >   _
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
        },
        "type": "int",
        "tail": "no"
      },
      {
        "start": {
          "line": 1,
          "col": 6
        },
        "end": {
          "line": 2,
          "col": 3
        },
        "type": "unit -> int",
        "tail": "no"
      }
    ],
    "notifications": []
  }

Check that we can access the expected type of a module hole:
  $ $MERLIN single type-enclosing -position 2:2 -filename hole.ml <<EOF
  > module M : sig val f : int -> unit end =
  >   _
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 9
        },
        "end": {
          "line": 2,
          "col": 3
        },
        "type": "sig val f : int -> unit end",
        "tail": "no"
      },
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 2,
          "col": 3
        },
        "type": "sig val f : int -> unit end",
        "tail": "no"
      }
    ],
    "notifications": []
  }

What about other places where Module_expr are allowed ?
  $ $MERLIN single type-enclosing -position 1:6 -filename hole.ml <<EOF
  > open _
  > EOF
  {
    "class": "return",
    "value": [],
    "notifications": []
  }

In includes:
  $ $MERLIN single type-enclosing -position 1:6 -filename hole.ml <<EOF
  > include _
  > EOF
  {
    "class": "return",
    "value": [],
    "notifications": []
  }

In module type of:
  $ $MERLIN single type-enclosing -verbosity 2 -position 1:12 -filename hole.ml <<EOF
  > module type Hole = module type of _
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 12
        },
        "end": {
          "line": 1,
          "col": 16
        },
        "type": "_",
        "tail": "no"
      },
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 1,
          "col": 35
        },
        "type": "_",
        "tail": "no"
      }
    ],
    "notifications": []
  }

In module coercions:
  $ $MERLIN single type-enclosing -position 2:16 -filename hole.ml <<EOF
  > module type Hole = module type of _
  > let m = (module _ : Hole)
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 16
        },
        "end": {
          "line": 2,
          "col": 17
        },
        "type": "Hole",
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 8
        },
        "end": {
          "line": 2,
          "col": 25
        },
        "type": "(module Hole)",
        "tail": "no"
      }
    ],
    "notifications": []
  }

Type when no type available ?
  $ $MERLIN single type-enclosing -position 1:8 -filename hole.ml <<EOF
  > module M = _
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 7
        },
        "end": {
          "line": 1,
          "col": 8
        },
        "type": "_",
        "tail": "no"
      },
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 1,
          "col": 12
        },
        "type": "_",
        "tail": "no"
      }
    ],
    "notifications": []
  }
