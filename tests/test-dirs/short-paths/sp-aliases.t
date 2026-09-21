  $ cat >test.ml <<'EOF'
  > type t = Foo
  > module X = struct
  >   type prev = t
  >   type t = Bar
  >   let err (_x : prev) = (Bar : t)
  > end
  > EOF

  $ echo "FLG -short-paths -nostdlib" > .merlin

  $ $MERLIN single type-enclosing -position 5:7 \
  > -filename test.ml < test.ml 2>&1| sed -E 's/^# [0-9]+.[0-9]+/#/'
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 5,
          "col": 6
        },
        "end": {
          "line": 5,
          "col": 9
        },
        "type": "prev -> t",
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 11
        },
        "end": {
          "line": 6,
          "col": 3
        },
        "type": "sig type prev = t type t = Bar val err : prev -> t end",
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 0
        },
        "end": {
          "line": 6,
          "col": 3
        },
        "type": "sig type prev = t type t = Bar val err : prev -> t end",
        "tail": "no"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single type-enclosing -position 5:7 \
  > -filename test.ml < test.ml 2>&1| sed -E 's/^# [0-9]+.[0-9]+/#/'
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 5,
          "col": 6
        },
        "end": {
          "line": 5,
          "col": 9
        },
        "type": "prev -> t",
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 11
        },
        "end": {
          "line": 6,
          "col": 3
        },
        "type": "sig type prev = t type t = Bar val err : prev -> t end",
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 0
        },
        "end": {
          "line": 6,
          "col": 3
        },
        "type": "sig type prev = t type t = Bar val err : prev -> t end",
        "tail": "no"
      }
    ],
    "notifications": []
  }


  $ $MERLIN single type-enclosing -position 2:0 \
  > -filename test.ml < test.ml | jq '.value[].type'
  "sig type prev = t type t = Bar val err : prev -> t end"
