Various parts of the letop together with andop:

- The let+ operator:
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
      "type": "'a option -> ('a -> 'b) -> 'b option",
      "tail": "no"
    },
    {
      "start": {
        "line": 8,
        "col": 2
      },
      "end": {
        "line": 8,
        "col": 6
      },
      "type": "((int * int) * int) option -> ((int * int) * int -> int) -> int option",
      "tail": "no"
    }
  ]
