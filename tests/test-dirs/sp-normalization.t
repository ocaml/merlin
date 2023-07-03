  $ cat >test.ml <<'EOF'
  > module Id = struct
  >   type 'a t = 'a
  > end
  > 
  > module Unit = struct
  >   type t = unit
  > end
  > 
  > type 'a t =
  >   | Id of 'a t Id.t
  >   | Unit of Unit.t
  > EOF

This query should not hang indefinitely
  $ $MERLIN single type-enclosing -position 11:17 -short-paths \
  > -filename test.ml < test.ml |
  > jq '.value'
  [
    {
      "start": {
        "line": 11,
        "col": 12
      },
      "end": {
        "line": 11,
        "col": 18
      },
      "type": "type t = unit",
      "tail": "no"
    },
    {
      "start": {
        "line": 11,
        "col": 12
      },
      "end": {
        "line": 11,
        "col": 18
      },
      "type": "unit",
      "tail": "no"
    },
    {
      "start": {
        "line": 9,
        "col": 0
      },
      "end": {
        "line": 11,
        "col": 18
      },
      "type": "type 'a t = Id of 'a t | Unit of unit",
      "tail": "no"
    }
  ]
