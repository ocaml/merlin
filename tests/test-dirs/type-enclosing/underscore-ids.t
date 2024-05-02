These tests ensure the stability of identifier reconstruction
in the presence of underscores.

1.1
  $ $MERLIN single type-enclosing -position 3:2 -filename under.ml <<EOF | \
  > jq '.value'
  > let _foo = 4.2
  > let f () : int =
  >   _foo
  > EOF
  [
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
  ]

1.2
  $ $MERLIN single type-enclosing -position 3:3 -filename under.ml <<EOF | \
  >  jq '.value'
  > let _foo = 4.2
  > let f () : int =
  >   _foo
  > EOF
  [
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
  ]

We try several places in the identifier to check the result stability
2.1
  $ $MERLIN single type-enclosing -position 3:5 -filename under.ml <<EOF | \
  >  jq '.value'
  > let foo_bar = 4.2
  > let f () : int =
  >   foo_bar
  > EOF
  [
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 9
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
        "col": 9
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
        "col": 9
      },
      "type": "unit -> int",
      "tail": "no"
    }
  ]

2.2
  $ $MERLIN single type-enclosing -position 3:6 -filename under.ml <<EOF | \
  >  jq '.value'
  > let foo_bar = 4.2
  > let f () : int =
  >   foo_bar
  > EOF
  [
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 9
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
        "col": 9
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
        "col": 9
      },
      "type": "unit -> int",
      "tail": "no"
    }
  ]

2.3
  $ $MERLIN single type-enclosing -position 3:7 -filename under.ml <<EOF | \
  >  jq '.value'
  > let foo_bar = 4.2
  > let f () : int =
  >   foo_bar
  > EOF
  [
    {
      "start": {
        "line": 3,
        "col": 2
      },
      "end": {
        "line": 3,
        "col": 9
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
        "col": 9
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
        "col": 9
      },
      "type": "unit -> int",
      "tail": "no"
    }
  ]

2.4
  $ $MERLIN single type-enclosing -position 3:8 -filename under.ml <<EOF
  > let foo_bar = 4.2
  > let f () : int =
  >   foo_bar
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
          "col": 9
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
          "col": 9
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
          "col": 9
        },
        "type": "unit -> int",
        "tail": "no"
      }
    ],
    "notifications": []
  }

3.1
  $ $MERLIN single type-enclosing -position 5:10 -filename under.ml <<EOF
  > let aa = 4.2
  > let f (x) : int = function
  >   | None -> 3
  >   | Some 5 -> 4
  >   | Some _aa -> 4
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 5,
          "col": 9
        },
        "end": {
          "line": 5,
          "col": 12
        },
        "type": "int",
        "tail": "no"
      },
      {
        "start": {
          "line": 5,
          "col": 4
        },
        "end": {
          "line": 5,
          "col": 12
        },
        "type": "int option",
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 6
        },
        "end": {
          "line": 5,
          "col": 17
        },
        "type": "'a -> int",
        "tail": "no"
      }
    ],
    "notifications": []
  }
