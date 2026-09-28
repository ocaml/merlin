  $ cat >main.ml <<'EOF'
  > let f ?x _ = x
  > 
  > let g childs = List.map f childs
  > EOF

FIXME: The first child of `g` in the response makes no sense.
  $ $MERLIN single outline < main.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 0
        },
        "end": {
          "line": 3,
          "col": 32
        },
        "name": "g",
        "kind": "Value",
        "type": "'a list -> 'b option list",
        "children": [
          {
            "start": {
              "line": 3,
              "col": 24
            },
            "end": {
              "line": 3,
              "col": 25
            },
            "name": "arg",
            "kind": "Value",
            "type": "?x:'a -> 'b -> 'a option",
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 0,
                "col": -1
              },
              "end": {
                "line": 0,
                "col": -1
              }
            }
          }
        ],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 3,
            "col": 4
          },
          "end": {
            "line": 3,
            "col": 5
          }
        }
      },
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 1,
          "col": 14
        },
        "name": "f",
        "kind": "Value",
        "type": "?x:'a -> 'b -> 'a option",
        "children": [],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 1,
            "col": 4
          },
          "end": {
            "line": 1,
            "col": 5
          }
        }
      }
    ],
    "notifications": []
  }
