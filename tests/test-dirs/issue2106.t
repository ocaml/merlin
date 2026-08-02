When `f` takes an optional argument and is passed to `List.map`, the
compiler eta-expands it; the generated binding carries no source
location and must not appear in the outline. Descendants coming from
the user's expression must survive the elision. See issue #2106.

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
        "children": [],
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

The elided binding's children belong to the user's expression and
must be hoisted, not dropped: `seed` below stays visible under `g`.

  $ cat >test2.ml <<EOF
  > let f ?x y = (x, y)
  > let g l = List.map (ignore (let seed = 1 in seed); f) l
  > EOF

  $ $MERLIN single outline -filename test2.ml <test2.ml |
  > jq '[.value[] | {name, children: [.children[].name]}]'
  [
    {
      "name": "g",
      "children": [
        "seed"
      ]
    },
    {
      "name": "f",
      "children": []
    }
  ]
