Test 1

  $ $MERLIN single type-enclosing  -position 2:5 -filename type_alias.ml <<EOF \
  > type foo = int  \
  > let x : foo = 1 \
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 4
        },
        "end": {
          "line": 2,
          "col": 5
        },
        "type": "foo",
        "tail": "no"
      }
    ],
    "notifications": []
  }

Test 2

  $ $MERLIN single type-enclosing  -position 2:5 -filename type_alias2.ml <<EOF \
  > type foo = int  \
  > let x = 1 \
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 4
        },
        "end": {
          "line": 2,
          "col": 5
        },
        "type": "int",
        "tail": "no"
      }
    ],
    "notifications": []
  }
