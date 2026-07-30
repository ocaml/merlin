The outline selection range must always be included in the item range
(the LSP protocol requires it). When `f` takes an optional argument and
is passed to `List.map`, the compiler eta-expands it; the generated
binding carries a dummy name location which must not leak into the
`selection` field. See issue #2106.

  $ cat >test.ml <<EOF
  > let f ?x _ = x
  > let g childs = List.map f childs
  > EOF

  $ $MERLIN single outline -filename test.ml <test.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 0
        },
        "end": {
          "line": 2,
          "col": 32
        },
        "name": "g",
        "kind": "Value",
        "type": "'a list -> 'b option list",
        "children": [
          {
            "start": {
              "line": 2,
              "col": 24
            },
            "end": {
              "line": 2,
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
            "line": 2,
            "col": 4
          },
          "end": {
            "line": 2,
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
