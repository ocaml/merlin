These tests ensure the stability of identifier reconstruction
in the presence of underscores.

  $ $MERLIN single type-enclosing -position 3:2 -filename under.ml <<EOF
  > let _foo = 4.2
  > let f () : int =
  >   _foo
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 2
        },
        "end": {
          "line": 3,
          "col": 6
        },
        "type": "float",
        "tail": "no"
      },
      {
        "start": {
          "line": 3,
          "col": 2
        },
        "end": {
          "line": 3,
          "col": 6
        },
        "type": "float",
        "tail": "no"
      },
      {
        "start": {
          "line": 3,
          "col": 2
        },
        "end": {
          "line": 3,
          "col": 6
        },
        "type": "int",
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 6
        },
        "end": {
          "line": 3,
          "col": 6
        },
        "type": "unit -> int",
        "tail": "no"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single type-enclosing -position 2:3 -filename under.ml <<EOF
  > let f () : int =
  >   _foo
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
          "col": 6
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
          "col": 6
        },
        "type": "unit -> int",
        "tail": "no"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single type-enclosing -position 2:4 -filename under.ml <<EOF
  > let f () : int =
  >   _foo
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
          "col": 6
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
          "col": 6
        },
        "type": "unit -> int",
        "tail": "no"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single type-enclosing -position 2:5 -filename under.ml <<EOF
  > let f () : int =
  >   foo_bar
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
          "col": 9
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
          "col": 9
        },
        "type": "unit -> int",
        "tail": "no"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single type-enclosing -position 2:6 -filename under.ml <<EOF
  > let f () : int =
  >   foo_bar
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
          "col": 9
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
          "col": 9
        },
        "type": "unit -> int",
        "tail": "no"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single type-enclosing -position 2:7 -filename under.ml <<EOF
  > let f () : int =
  >   foo_bar
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
          "col": 9
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
          "col": 9
        },
        "type": "unit -> int",
        "tail": "no"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single type-enclosing -position 2:8 -filename under.ml <<EOF
  > let f () : int =
  >   foo_bar
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
          "col": 9
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
          "col": 9
        },
        "type": "unit -> int",
        "tail": "no"
      }
    ],
    "notifications": []
  }


  $ $MERLIN single type-enclosing -position 3:10 -filename under.ml <<EOF
  > let f (x) : int = function
  >   | None -> 3
  >   | Some _aa -> 4
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 18
        },
        "end": {
          "line": 3,
          "col": 17
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
          "line": 3,
          "col": 17
        },
        "type": "'a -> int",
        "tail": "no"
      }
    ],
    "notifications": []
  }
