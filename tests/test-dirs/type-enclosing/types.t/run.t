  $ $MERLIN single type-enclosing -position 5:11 -verbosity 0 \
  > -filename ./types.ml < ./types.ml | jq ".value[0:2]"
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
  > -filename ./types.ml < ./types.ml | jq ".value[0:2]"
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

FIXME: A type with a type param shouldn't equal itself - 1

  $ $MERLIN single type-enclosing -position 7:9 -verbosity 0 \
  > -filename ./types.ml < ./types.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 7,
        "col": 8
      },
      "end": {
        "line": 7,
        "col": 9
      },
      "type": "type 'a t = 'b t",
      "tail": "no"
    },
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

FIXME: A type with a type param shouldn't equal itself - aliasing a user-defined type

  $ $MERLIN single type-enclosing -position 9:9 -verbosity 0 \
  > -filename ./types.ml < ./types.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 9,
        "col": 8
      },
      "end": {
        "line": 9,
        "col": 9
      },
      "type": "type 'a s = 'b s",
      "tail": "no"
    },
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
  > -filename ./types.ml < ./types.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 11,
        "col": 8
      },
      "end": {
        "line": 11,
        "col": 9
      },
      "type": "type 'a l = 'b l",
      "tail": "no"
    },
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

FIXME: A type with a type param shouldn't be empty

  $ $MERLIN single type-enclosing -short-paths -position 17:9 -verbosity 0 \
  > -filename ./types.ml < ./types.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 17,
        "col": 8
      },
      "end": {
        "line": 17,
        "col": 9
      },
      "type": "type 'a v",
      "tail": "no"
    },
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


