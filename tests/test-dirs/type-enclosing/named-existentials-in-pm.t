  $ $MERLIN single type-enclosing -position 3:59 -filename test.ml <<EOF
  > type _ ty = Int : int ty
  > type dyn = Dyn : 'a ty * 'a -> dyn
  > let f = function Dyn (type a) (w, x : a ty * a) -> ignore (x : a)
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 59
        },
        "end": {
          "line": 3,
          "col": 60
        },
        "type": "a",
        "tail": "no"
      },
      {
        "start": {
          "line": 3,
          "col": 59
        },
        "end": {
          "line": 3,
          "col": 60
        },
        "type": "a",
        "tail": "no"
      },
      {
        "start": {
          "line": 3,
          "col": 51
        },
        "end": {
          "line": 3,
          "col": 65
        },
        "type": "unit",
        "tail": "no"
      },
      {
        "start": {
          "line": 3,
          "col": 8
        },
        "end": {
          "line": 3,
          "col": 65
        },
        "type": "dyn -> unit",
        "tail": "no"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single type-enclosing -position 3:63 -filename test.ml <<EOF
  > type _ ty = Int : int ty
  > type dyn = Dyn : 'a ty * 'a -> dyn
  > let f = function Dyn (type a) (w, x : a ty * a) -> ignore (x : a)
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 63
        },
        "end": {
          "line": 3,
          "col": 64
        },
        "type": "type a",
        "tail": "no"
      },
      {
        "start": {
          "line": 3,
          "col": 63
        },
        "end": {
          "line": 3,
          "col": 64
        },
        "type": "a",
        "tail": "no"
      },
      {
        "start": {
          "line": 3,
          "col": 51
        },
        "end": {
          "line": 3,
          "col": 65
        },
        "type": "unit",
        "tail": "no"
      },
      {
        "start": {
          "line": 3,
          "col": 8
        },
        "end": {
          "line": 3,
          "col": 65
        },
        "type": "dyn -> unit",
        "tail": "no"
      }
    ],
    "notifications": []
  }
