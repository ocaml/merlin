  $ cat >main.ml <<'EOF'
  > let _ = List.map Fun.id [3]
  > EOF

With index 0 only the first type is shown:
  $ $MERLIN single type-enclosing -position 1:14 -index 0 \
  > -filename ./main.ml < ./main.ml | jq '.value[0,1]'
  {
    "start": {
      "line": 1,
      "col": 8
    },
    "end": {
      "line": 1,
      "col": 16
    },
    "type": "('a -> 'b) -> 'a list -> 'b list",
    "tail": "no"
  }
  {
    "start": {
      "line": 1,
      "col": 8
    },
    "end": {
      "line": 1,
      "col": 16
    },
    "type": 1,
    "tail": "no"
  }

With index 1 only the second is shown (the first is a string so it is always shown):
  $ $MERLIN single type-enclosing -position 1:14 -index 1 \
  > -filename ./main.ml < ./main.ml  | jq '.value[0,1]'
  {
    "start": {
      "line": 1,
      "col": 8
    },
    "end": {
      "line": 1,
      "col": 16
    },
    "type": "('a -> 'b) -> 'a list -> 'b list",
    "tail": "no"
  }
  {
    "start": {
      "line": 1,
      "col": 8
    },
    "end": {
      "line": 1,
      "col": 16
    },
    "type": "(int -> int) -> int list -> int list",
    "tail": "no"
  }


With index 0 only the first type is shown:
  $ $MERLIN single type-enclosing -position 1:10 -index 0 \
  > -filename ./main.ml < ./main.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 8
        },
        "end": {
          "line": 1,
          "col": 12
        },
        "type": "(module Stdlib__List)",
        "tail": "no"
      },
      {
        "start": {
          "line": 1,
          "col": 8
        },
        "end": {
          "line": 1,
          "col": 16
        },
        "type": 1,
        "tail": "no"
      },
      {
        "start": {
          "line": 1,
          "col": 8
        },
        "end": {
          "line": 1,
          "col": 27
        },
        "type": 2,
        "tail": "no"
      }
    ],
    "notifications": []
  }

With index 1 only the second is shown (the first is a string so it is always shown):
FIXME? We don't see the generic version
  $ $MERLIN single type-enclosing -position 1:10 -index 1 \
  > -filename ./main.ml < ./main.ml 
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 8
        },
        "end": {
          "line": 1,
          "col": 12
        },
        "type": "(module Stdlib__List)",
        "tail": "no"
      },
      {
        "start": {
          "line": 1,
          "col": 8
        },
        "end": {
          "line": 1,
          "col": 16
        },
        "type": "(int -> int) -> int list -> int list",
        "tail": "no"
      },
      {
        "start": {
          "line": 1,
          "col": 8
        },
        "end": {
          "line": 1,
          "col": 27
        },
        "type": 2,
        "tail": "no"
      }
    ],
    "notifications": []
  }

  $ cat >main.ml <<'EOF'
  > module List = struct let map : (int -> int) -> int list -> int list = List.map end
  > let _ = List.map Fun.id [3]
  > EOF

FIXME With index 0 only the first type is shown but deduplication failed becauser the
next type was not rendered.
  $ $MERLIN single type-enclosing -position 2:14 -index 0 \
  > -filename ./main.ml < ./main.ml 
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 8
        },
        "end": {
          "line": 2,
          "col": 16
        },
        "type": "(int -> int) -> int list -> int list",
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 8
        },
        "end": {
          "line": 2,
          "col": 16
        },
        "type": 1,
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 8
        },
        "end": {
          "line": 2,
          "col": 27
        },
        "type": 2,
        "tail": "no"
      }
    ],
    "notifications": []
  }

FIXME With index 1 the list is shorter and the numbering is wrong ! In fact, it
should have been shorter earlier.
  $ $MERLIN single type-enclosing -position 2:14 -index 1 \
  > -filename ./main.ml < ./main.ml 
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 8
        },
        "end": {
          "line": 2,
          "col": 16
        },
        "type": "(int -> int) -> int list -> int list",
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 8
        },
        "end": {
          "line": 2,
          "col": 27
        },
        "type": 2,
        "tail": "no"
      }
    ],
    "notifications": []
  }
