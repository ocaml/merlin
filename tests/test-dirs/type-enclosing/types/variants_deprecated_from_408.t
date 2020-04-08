(enabled_if (>= %{ocaml_version} 4.08.0))

  $ $MERLIN single type-enclosing -position 2:15 -verbosity 0 \
  > -filename ./variants.ml < ./variants.ml | jq ".value[0:2]"
  Alert deprecated: old syntax for polymorphic variant type
  Alert deprecated: old syntax for polymorphic variant type
  [
    {
      "start": {
        "line": 2,
        "col": 14
      },
      "end": {
        "line": 2,
        "col": 18
      },
      "type": "type core = [ `A | `B ]",
      "tail": "no"
    },
    {
      "start": {
        "line": 2,
        "col": 14
      },
      "end": {
        "line": 2,
        "col": 18
      },
      "type": "core",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 2:13 -verbosity 0 \
  > -filename ./variants.ml < ./variants.ml | jq ".value[0:2]"
  Alert deprecated: old syntax for polymorphic variant type
  Alert deprecated: old syntax for polymorphic variant type
  [
    {
      "start": {
        "line": 2,
        "col": 12
      },
      "end": {
        "line": 2,
        "col": 25
      },
      "type": "[ `A | `B | `C ]",
      "tail": "no"
    },
    {
      "start": {
        "line": 2,
        "col": 0
      },
      "end": {
        "line": 2,
        "col": 25
      },
      "type": "type more = [ `A | `B | `C ]",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 4:5 -verbosity 0 \
  > -filename ./variants.ml < ./variants.ml | jq ".value[0:2]"
  Alert deprecated: old syntax for polymorphic variant type
  Alert deprecated: old syntax for polymorphic variant type
  [
    {
      "start": {
        "line": 4,
        "col": 4
      },
      "end": {
        "line": 4,
        "col": 5
      },
      "type": "more",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 4:5 -verbosity 1 \
  > -filename ./variants.ml < ./variants.ml | jq ".value[0:2]"
  Alert deprecated: old syntax for polymorphic variant type
  Alert deprecated: old syntax for polymorphic variant type
  [
    {
      "start": {
        "line": 4,
        "col": 4
      },
      "end": {
        "line": 4,
        "col": 5
      },
      "type": "[ `A | `B | `C ]",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 4:5 -verbosity 0 \
  > -filename ./variants.ml < ./variants.ml | jq ".value[0:2]"
  Alert deprecated: old syntax for polymorphic variant type
  Alert deprecated: old syntax for polymorphic variant type
  [
    {
      "start": {
        "line": 4,
        "col": 4
      },
      "end": {
        "line": 4,
        "col": 5
      },
      "type": "more",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 5:9 -verbosity 0 \
  > -filename ./variants.ml < ./variants.ml | jq ".value[0:2]"
  Alert deprecated: old syntax for polymorphic variant type
  Alert deprecated: old syntax for polymorphic variant type
  [
    {
      "start": {
        "line": 5,
        "col": 9
      },
      "end": {
        "line": 5,
        "col": 13
      },
      "type": "type core = [ `A | `B ]",
      "tail": "no"
    },
    {
      "start": {
        "line": 5,
        "col": 8
      },
      "end": {
        "line": 5,
        "col": 13
      },
      "type": "[< core > `B ]",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 5:17 -verbosity 0 \
  > -filename ./variants.ml < ./variants.ml | jq ".value[0:2]"
  Alert deprecated: old syntax for polymorphic variant type
  Alert deprecated: old syntax for polymorphic variant type
  [
    {
      "start": {
        "line": 5,
        "col": 16
      },
      "end": {
        "line": 5,
        "col": 18
      },
      "type": "[< core > `B ]",
      "tail": "no"
    }
  ]

FIXME: Not satisfying, expected core not more
  $ $MERLIN single type-enclosing -position 9:3 -verbosity 0 \
  > -filename ./variants.ml < ./variants.ml | jq ".value[0:2]"
  Alert deprecated: old syntax for polymorphic variant type
  Alert deprecated: old syntax for polymorphic variant type
  [
    {
      "start": {
        "line": 9,
        "col": 2
      },
      "end": {
        "line": 9,
        "col": 7
      },
      "type": "more",
      "tail": "no"
    },
    {
      "start": {
        "line": 7,
        "col": 9
      },
      "end": {
        "line": 9,
        "col": 13
      },
      "type": "unit",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 9:3 -verbosity 1 \
  > -filename ./variants.ml < ./variants.ml | jq ".value[0:2]"
  Alert deprecated: old syntax for polymorphic variant type
  Alert deprecated: old syntax for polymorphic variant type
  [
    {
      "start": {
        "line": 9,
        "col": 2
      },
      "end": {
        "line": 9,
        "col": 7
      },
      "type": "[ `A | `B | `C ]",
      "tail": "no"
    },
    {
      "start": {
        "line": 7,
        "col": 9
      },
      "end": {
        "line": 9,
        "col": 13
      },
      "type": "type unit = ()",
      "tail": "no"
    }
  ]
