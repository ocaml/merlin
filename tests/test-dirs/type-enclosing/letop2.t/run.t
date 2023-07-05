Various parts of the letop together with andop:

- The first let+ operator:

TODO: Type is wrong here, shouldn't be int * int

  $ $MERLIN single type-enclosing -position 8:4 -verbosity 0 \
  > -filename ./letop.ml < ./letop.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 8,
        "col": 2
      },
      "end": {
        "line": 8,
        "col": 6
      },
      "type": "int * int",
      "tail": "no"
    },
    {
      "start": {
        "line": 8,
        "col": 2
      },
      "end": {
        "line": 10,
        "col": 15
      },
      "type": "int option",
      "tail": "no"
    }
  ]

- The pattern:

TODO: type is wrong; range is wrong

  $ $MERLIN single type-enclosing -position 8:9 -verbosity 0 \
  > -filename ./letop.ml < ./letop.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 8,
        "col": 2
      },
      "end": {
        "line": 10,
        "col": 15
      },
      "type": "int option",
      "tail": "no"
    },
    {
      "start": {
        "line": 7,
        "col": 29
      },
      "end": {
        "line": 10,
        "col": 15
      },
      "type": "'a * 'b -> int option",
      "tail": "no"
    }
  ]

- The rhs:

  $ $MERLIN single type-enclosing -position 8:22 -verbosity 0 \
  > -filename ./letop.ml < ./letop.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 8,
        "col": 13
      },
      "end": {
        "line": 8,
        "col": 29
      },
      "type": "('a, 'b) Hashtbl.t -> 'a -> 'b option",
      "tail": "no"
    },
    {
      "start": {
        "line": 8,
        "col": 13
      },
      "end": {
        "line": 8,
        "col": 29
      },
      "type": "('a, int) Hashtbl.t -> 'a -> int option",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 8:31 -verbosity 0 \
  > -filename ./letop.ml < ./letop.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 8,
        "col": 30
      },
      "end": {
        "line": 8,
        "col": 34
      },
      "type": "('a, int) Hashtbl.t",
      "tail": "no"
    },
    {
      "start": {
        "line": 8,
        "col": 13
      },
      "end": {
        "line": 8,
        "col": 39
      },
      "type": "int option",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 8:36 -verbosity 0 \
  > -filename ./letop.ml < ./letop.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 8,
        "col": 35
      },
      "end": {
        "line": 8,
        "col": 39
      },
      "type": "'a",
      "tail": "no"
    },
    {
      "start": {
        "line": 8,
        "col": 13
      },
      "end": {
        "line": 8,
        "col": 39
      },
      "type": "int option",
      "tail": "no"
    }
  ]

- The second and+ operator:

TODO: type is wrong; range is wrong

  $ $MERLIN single type-enclosing -position 9:4 -verbosity 0 \
  > -filename ./letop.ml < ./letop.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 8,
        "col": 2
      },
      "end": {
        "line": 10,
        "col": 15
      },
      "type": "int option",
      "tail": "no"
    },
    {
      "start": {
        "line": 7,
        "col": 29
      },
      "end": {
        "line": 10,
        "col": 15
      },
      "type": "'a * 'b -> int option",
      "tail": "no"
    }
  ]

- The pattern:

TODO: type is wrong; range is wrong

  $ $MERLIN single type-enclosing -position 9:9 -verbosity 0 \
  > -filename ./letop.ml < ./letop.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 8,
        "col": 2
      },
      "end": {
        "line": 10,
        "col": 15
      },
      "type": "int option",
      "tail": "no"
    },
    {
      "start": {
        "line": 7,
        "col": 29
      },
      "end": {
        "line": 10,
        "col": 15
      },
      "type": "'a * 'b -> int option",
      "tail": "no"
    }
  ]

- The rhs:

  $ $MERLIN single type-enclosing -position 9:22 -verbosity 0 \
  > -filename ./letop.ml < ./letop.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 9,
        "col": 13
      },
      "end": {
        "line": 9,
        "col": 29
      },
      "type": "('a, 'b) Hashtbl.t -> 'a -> 'b option",
      "tail": "no"
    },
    {
      "start": {
        "line": 9,
        "col": 13
      },
      "end": {
        "line": 9,
        "col": 29
      },
      "type": "('a, int) Hashtbl.t -> 'a -> int option",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 8:31 -verbosity 0 \
  > -filename ./letop.ml < ./letop.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 8,
        "col": 30
      },
      "end": {
        "line": 8,
        "col": 34
      },
      "type": "('a, int) Hashtbl.t",
      "tail": "no"
    },
    {
      "start": {
        "line": 8,
        "col": 13
      },
      "end": {
        "line": 8,
        "col": 39
      },
      "type": "int option",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 8:36 -verbosity 0 \
  > -filename ./letop.ml < ./letop.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 8,
        "col": 35
      },
      "end": {
        "line": 8,
        "col": 39
      },
      "type": "'a",
      "tail": "no"
    },
    {
      "start": {
        "line": 8,
        "col": 13
      },
      "end": {
        "line": 8,
        "col": 39
      },
      "type": "int option",
      "tail": "no"
    }
  ]

- After the in:

  $ $MERLIN single type-enclosing -position 9:42 -verbosity 0 \
  > -filename ./letop.ml < ./letop.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 8,
        "col": 2
      },
      "end": {
        "line": 10,
        "col": 15
      },
      "type": "int option",
      "tail": "no"
    },
    {
      "start": {
        "line": 7,
        "col": 29
      },
      "end": {
        "line": 10,
        "col": 15
      },
      "type": "'a * 'b -> int option",
      "tail": "no"
    }
  ]

