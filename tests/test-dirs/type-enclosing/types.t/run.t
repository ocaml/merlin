  $ $MERLIN single type-enclosing -position 5:11 -verbosity 0 \
  > -filename ./types.ml < ./types.ml | jq ".value"
  [
    {
      "start": {
        "line": 5,
        "col": 10
      },
      "end": {
        "line": 5,
        "col": 11
      },
      "type": "type x = Foo",
      "tail": "no"
    },
    {
      "start": {
        "line": 5,
        "col": 10
      },
      "end": {
        "line": 5,
        "col": 11
      },
      "type": "x",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 5:11 -verbosity 1 \
  > -filename ./types.ml < ./types.ml | jq ".value"
  [
    {
      "start": {
        "line": 5,
        "col": 10
      },
      "end": {
        "line": 5,
        "col": 11
      },
      "type": "type x = Foo",
      "tail": "no"
    },
    {
      "start": {
        "line": 5,
        "col": 10
      },
      "end": {
        "line": 5,
        "col": 11
      },
      "type": "type x = Foo",
      "tail": "no"
    }
  ]

FIXED: small enclosing was incorrect?

  $ $MERLIN single type-enclosing -position 7:9 -verbosity 0 \
  > -filename ./types.ml < ./types.ml | jq ".value"
  [
    {
      "start": {
        "line": 7,
        "col": 0
      },
      "end": {
        "line": 7,
        "col": 14
      },
      "type": "type 'a t = 'a",
      "tail": "no"
    }
  ]

FIXED: small enclosing was incorrect?

  $ $MERLIN single type-enclosing -position 9:9 -verbosity 0 \
  > -filename ./types.ml < ./types.ml | jq ".value"
  [
    {
      "start": {
        "line": 9,
        "col": 0
      },
      "end": {
        "line": 9,
        "col": 16
      },
      "type": "type 'a s = 'a t",
      "tail": "no"
    }
  ]

FIXME: A type with a type param shouldn't equal itself - aliasing a list type

  $ $MERLIN single type-enclosing -short-paths -position 11:9 -verbosity 0 \
  > -filename ./types.ml < ./types.ml | jq ".value"
  [
    {
      "start": {
        "line": 11,
        "col": 0
      },
      "end": {
        "line": 11,
        "col": 19
      },
      "type": "type 'a l = 'a l",
      "tail": "no"
    }
  ]

Same result regardless of verbosity:

  $ $MERLIN single type-enclosing -short-paths -position 11:9 -verbosity 1 \
  > -filename ./types.ml < ./types.ml | jq ".value"
  [
    {
      "start": {
        "line": 11,
        "col": 0
      },
      "end": {
        "line": 11,
        "col": 19
      },
      "type": "type 'a l = 'a l",
      "tail": "no"
    }
  ]

OK without -short-paths

  $ $MERLIN single type-enclosing -position 11:9 -verbosity 0 \
  > -filename ./types.ml < ./types.ml | jq ".value"
  [
    {
      "start": {
        "line": 11,
        "col": 0
      },
      "end": {
        "line": 11,
        "col": 19
      },
      "type": "type 'a l = 'a list",
      "tail": "no"
    }
  ]


FIXED: small enclosing at EOF was incorrect?

  $ $MERLIN single type-enclosing -short-paths -position 17:9 -verbosity 0 \
  > -filename ./types.ml < ./types.ml | jq ".value"
  [
    {
      "start": {
        "line": 17,
        "col": 0
      },
      "end": {
        "line": 17,
        "col": 21
      },
      "type": "type 'a v = Foo of 'a",
      "tail": "no"
    }
  ]


