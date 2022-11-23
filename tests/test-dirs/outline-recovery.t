  $ cat >test.ml <<EOF
  > module Make = struct
  >   module A = struct end
  >   module B = C.C1
  >   module D = struct end
  > end
  > EOF

  $ $MERLIN single outline -filename test.ml <test.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 5,
          "col": 3
        },
        "name": "Make",
        "kind": "Module",
        "type": null,
        "children": [
          {
            "start": {
              "line": 4,
              "col": 2
            },
            "end": {
              "line": 4,
              "col": 23
            },
            "name": "D",
            "kind": "Module",
            "type": null,
            "children": [],
            "deprecated": false
          },
          {
            "start": {
              "line": 3,
              "col": 2
            },
            "end": {
              "line": 3,
              "col": 17
            },
            "name": "B",
            "kind": "Module",
            "type": null,
            "children": [],
            "deprecated": false
          },
          {
            "start": {
              "line": 2,
              "col": 2
            },
            "end": {
              "line": 2,
              "col": 23
            },
            "name": "A",
            "kind": "Module",
            "type": null,
            "children": [],
            "deprecated": false
          }
        ],
        "deprecated": false
      }
    ],
    "notifications": []
  }
