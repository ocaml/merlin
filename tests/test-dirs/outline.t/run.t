  $ echo "S .\nB .\nFLG -nopervasives" > .merlin
  $ $MERLIN single outline < foo.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 67,
          "col": 0
        },
        "end": {
          "line": 73,
          "col": 3
        },
        "name": "final_let",
        "kind": "Value",
        "type": "< foo : int >",
        "children": [
          {
            "start": {
              "line": 68,
              "col": 2
            },
            "end": {
              "line": 71,
              "col": 7
            },
            "name": "c",
            "kind": "Value",
            "type": null,
            "children": [
              {
                "start": {
                  "line": 70,
                  "col": 13
                },
                "end": {
                  "line": 70,
                  "col": 16
                },
                "name": "foo",
                "kind": "Method",
                "type": null,
                "children": [],
                "deprecated": false
              }
            ],
            "deprecated": false
          }
        ],
        "deprecated": false
      },
      {
        "start": {
          "line": 65,
          "col": 0
        },
        "end": {
          "line": 65,
          "col": 19
        },
        "name": "tb",
        "kind": "ClassType",
        "type": null,
        "children": [],
        "deprecated": false
      },
      {
        "start": {
          "line": 61,
          "col": 0
        },
        "end": {
          "line": 63,
          "col": 3
        },
        "name": "ta",
        "kind": "ClassType",
        "type": null,
        "children": [
          {
            "start": {
              "line": 62,
              "col": 2
            },
            "end": {
              "line": 62,
              "col": 35
            },
            "name": "baz",
            "kind": "Method",
            "type": null,
            "children": [],
            "deprecated": false
          }
        ],
        "deprecated": false
      },
      {
        "start": {
          "line": 55,
          "col": 0
        },
        "end": {
          "line": 59,
          "col": 5
        },
        "name": "b",
        "kind": "Class",
        "type": null,
        "children": [
          {
            "start": {
              "line": 58,
              "col": 11
            },
            "end": {
              "line": 58,
              "col": 14
            },
            "name": "bar",
            "kind": "Method",
            "type": null,
            "children": [],
            "deprecated": false
          },
          {
            "start": {
              "line": 57,
              "col": 8
            },
            "end": {
              "line": 57,
              "col": 11
            },
            "name": "foo",
            "kind": "Value",
            "type": null,
            "children": [],
            "deprecated": false
          }
        ],
        "deprecated": false
      },
      {
        "start": {
          "line": 45,
          "col": 0
        },
        "end": {
          "line": 53,
          "col": 5
        },
        "name": "a",
        "kind": "Class",
        "type": null,
        "children": [
          {
            "start": {
              "line": 47,
              "col": 8
            },
            "end": {
              "line": 47,
              "col": 9
            },
            "name": "b",
            "kind": "Value",
            "type": null,
            "children": [
              {
                "start": {
                  "line": 49,
                  "col": 15
                },
                "end": {
                  "line": 49,
                  "col": 25
                },
                "name": "inside_a_b",
                "kind": "Method",
                "type": null,
                "children": [
                  {
                    "start": {
                      "line": 50,
                      "col": 10
                    },
                    "end": {
                      "line": 50,
                      "col": 31
                    },
                    "name": "x_inside_a_b",
                    "kind": "Value",
                    "type": null,
                    "children": [],
                    "deprecated": false
                  }
                ],
                "deprecated": false
              }
            ],
            "deprecated": false
          }
        ],
        "deprecated": false
      },
      {
        "start": {
          "line": 43,
          "col": 0
        },
        "end": {
          "line": 43,
          "col": 18
        },
        "name": "c",
        "kind": "Class",
        "type": null,
        "children": [],
        "deprecated": false
      },
      {
        "start": {
          "line": 37,
          "col": 0
        },
        "end": {
          "line": 41,
          "col": 5
        },
        "name": "b",
        "kind": "Class",
        "type": null,
        "children": [
          {
            "start": {
              "line": 40,
              "col": 11
            },
            "end": {
              "line": 40,
              "col": 14
            },
            "name": "bar",
            "kind": "Method",
            "type": null,
            "children": [],
            "deprecated": false
          },
          {
            "start": {
              "line": 39,
              "col": 8
            },
            "end": {
              "line": 39,
              "col": 11
            },
            "name": "foo",
            "kind": "Value",
            "type": null,
            "children": [],
            "deprecated": false
          }
        ],
        "deprecated": false
      },
      {
        "start": {
          "line": 35,
          "col": 0
        },
        "end": {
          "line": 35,
          "col": 19
        },
        "name": "tb",
        "kind": "ClassType",
        "type": null,
        "children": [],
        "deprecated": false
      },
      {
        "start": {
          "line": 33,
          "col": 0
        },
        "end": {
          "line": 33,
          "col": 26
        },
        "name": "ta",
        "kind": "ClassType",
        "type": null,
        "children": [],
        "deprecated": false
      },
      {
        "start": {
          "line": 31,
          "col": 0
        },
        "end": {
          "line": 31,
          "col": 18
        },
        "name": "c",
        "kind": "Class",
        "type": null,
        "children": [],
        "deprecated": false
      },
      {
        "start": {
          "line": 29,
          "col": 0
        },
        "end": {
          "line": 29,
          "col": 18
        },
        "name": "b",
        "kind": "Class",
        "type": null,
        "children": [],
        "deprecated": false
      },
      {
        "start": {
          "line": 27,
          "col": 0
        },
        "end": {
          "line": 27,
          "col": 20
        },
        "name": "a",
        "kind": "Class",
        "type": null,
        "children": [],
        "deprecated": false
      },
      {
        "start": {
          "line": 25,
          "col": 0
        },
        "end": {
          "line": 25,
          "col": 42
        },
        "name": "point",
        "kind": "Type",
        "type": null,
        "children": [
          {
            "start": {
              "line": 25,
              "col": 34
            },
            "end": {
              "line": 25,
              "col": 40
            },
            "name": "z",
            "kind": "Label",
            "type": null,
            "children": [],
            "deprecated": false
          },
          {
            "start": {
              "line": 25,
              "col": 26
            },
            "end": {
              "line": 25,
              "col": 33
            },
            "name": "y",
            "kind": "Label",
            "type": null,
            "children": [],
            "deprecated": false
          },
          {
            "start": {
              "line": 25,
              "col": 18
            },
            "end": {
              "line": 25,
              "col": 25
            },
            "name": "x",
            "kind": "Label",
            "type": null,
            "children": [],
            "deprecated": false
          }
        ],
        "deprecated": false
      },
      {
        "start": {
          "line": 23,
          "col": 0
        },
        "end": {
          "line": 23,
          "col": 50
        },
        "name": "eithery",
        "kind": "Type",
        "type": null,
        "children": [
          {
            "start": {
              "line": 23,
              "col": 36
            },
            "end": {
              "line": 23,
              "col": 50
            },
            "name": "Righty",
            "kind": "Constructor",
            "type": null,
            "children": [],
            "deprecated": false
          },
          {
            "start": {
              "line": 23,
              "col": 24
            },
            "end": {
              "line": 23,
              "col": 35
            },
            "name": "Lefty",
            "kind": "Constructor",
            "type": null,
            "children": [],
            "deprecated": false
          }
        ],
        "deprecated": false
      },
      {
        "start": {
          "line": 21,
          "col": 0
        },
        "end": {
          "line": 21,
          "col": 20
        },
        "name": "Ex",
        "kind": "Exn",
        "type": null,
        "children": [],
        "deprecated": false
      },
      {
        "start": {
          "line": 16,
          "col": 0
        },
        "end": {
          "line": 19,
          "col": 5
        },
        "name": "class_b",
        "kind": "Class",
        "type": null,
        "children": [
          {
            "start": {
              "line": 18,
              "col": 11
            },
            "end": {
              "line": 18,
              "col": 12
            },
            "name": "b",
            "kind": "Method",
            "type": null,
            "children": [],
            "deprecated": false
          }
        ],
        "deprecated": false
      },
      {
        "start": {
          "line": 12,
          "col": 0
        },
        "end": {
          "line": 14,
          "col": 3
        },
        "name": "class_type_a",
        "kind": "ClassType",
        "type": null,
        "children": [
          {
            "start": {
              "line": 13,
              "col": 2
            },
            "end": {
              "line": 13,
              "col": 23
            },
            "name": "a",
            "kind": "Method",
            "type": null,
            "children": [],
            "deprecated": false
          }
        ],
        "deprecated": false
      },
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 10,
          "col": 3
        },
        "name": "Bar",
        "kind": "Module",
        "type": null,
        "children": [
          {
            "start": {
              "line": 9,
              "col": 2
            },
            "end": {
              "line": 9,
              "col": 27
            },
            "name": "b",
            "kind": "ClassType",
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
              "line": 7,
              "col": 5
            },
            "name": "S1",
            "kind": "Signature",
            "type": null,
            "children": [
              {
                "start": {
                  "line": 6,
                  "col": 4
                },
                "end": {
                  "line": 6,
                  "col": 22
                },
                "name": "foo",
                "kind": "Value",
                "type": "t -> int",
                "children": [],
                "deprecated": false
              },
              {
                "start": {
                  "line": 4,
                  "col": 4
                },
                "end": {
                  "line": 4,
                  "col": 10
                },
                "name": "t",
                "kind": "Type",
                "type": null,
                "children": [],
                "deprecated": false
              }
            ],
            "deprecated": false
          },
          {
            "start": {
              "line": 2,
              "col": 2
            },
            "end": {
              "line": 2,
              "col": 14
            },
            "name": "t",
            "kind": "Type",
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
  $ $MERLIN single outline < path.ml | jq '.value[].type'
  "A.a"
  null
  $ $MERLIN single outline -short-paths < path.ml | jq '.value[].type'
  "a"
  null
