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
            "type": "< foo : int >",
            "children": [
              {
                "start": {
                  "line": 70,
                  "col": 6
                },
                "end": {
                  "line": 70,
                  "col": 21
                },
                "name": "foo",
                "kind": "Method",
                "type": null,
                "children": [],
                "deprecated": false,
                "selection": {
                  "start": {
                    "line": 70,
                    "col": 13
                  },
                  "end": {
                    "line": 70,
                    "col": 16
                  }
                }
              }
            ],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 68,
                "col": 6
              },
              "end": {
                "line": 68,
                "col": 7
              }
            }
          }
        ],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 67,
            "col": 4
          },
          "end": {
            "line": 67,
            "col": 13
          }
        }
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
        "deprecated": false,
        "selection": {
          "start": {
            "line": 65,
            "col": 4
          },
          "end": {
            "line": 65,
            "col": 6
          }
        }
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
            "deprecated": false,
            "selection": {
              "start": {
                "line": 62,
                "col": 2
              },
              "end": {
                "line": 62,
                "col": 35
              }
            }
          }
        ],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 61,
            "col": 11
          },
          "end": {
            "line": 61,
            "col": 13
          }
        }
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
              "col": 4
            },
            "end": {
              "line": 58,
              "col": 36
            },
            "name": "bar",
            "kind": "Method",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 58,
                "col": 11
              },
              "end": {
                "line": 58,
                "col": 14
              }
            }
          },
          {
            "start": {
              "line": 57,
              "col": 4
            },
            "end": {
              "line": 57,
              "col": 16
            },
            "name": "foo",
            "kind": "Value",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 57,
                "col": 8
              },
              "end": {
                "line": 57,
                "col": 11
              }
            }
          }
        ],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 55,
            "col": 4
          },
          "end": {
            "line": 55,
            "col": 5
          }
        }
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
              "col": 4
            },
            "end": {
              "line": 52,
              "col": 9
            },
            "name": "b",
            "kind": "Value",
            "type": null,
            "children": [
              {
                "start": {
                  "line": 49,
                  "col": 8
                },
                "end": {
                  "line": 51,
                  "col": 32
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
                    "type": "int",
                    "children": [],
                    "deprecated": false,
                    "selection": {
                      "start": {
                        "line": 50,
                        "col": 14
                      },
                      "end": {
                        "line": 50,
                        "col": 26
                      }
                    }
                  }
                ],
                "deprecated": false,
                "selection": {
                  "start": {
                    "line": 49,
                    "col": 15
                  },
                  "end": {
                    "line": 49,
                    "col": 25
                  }
                }
              }
            ],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 47,
                "col": 8
              },
              "end": {
                "line": 47,
                "col": 9
              }
            }
          }
        ],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 45,
            "col": 6
          },
          "end": {
            "line": 45,
            "col": 7
          }
        }
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
        "deprecated": false,
        "selection": {
          "start": {
            "line": 43,
            "col": 4
          },
          "end": {
            "line": 43,
            "col": 5
          }
        }
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
              "col": 4
            },
            "end": {
              "line": 40,
              "col": 39
            },
            "name": "bar",
            "kind": "Method",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 40,
                "col": 11
              },
              "end": {
                "line": 40,
                "col": 14
              }
            }
          },
          {
            "start": {
              "line": 39,
              "col": 4
            },
            "end": {
              "line": 39,
              "col": 16
            },
            "name": "foo",
            "kind": "Value",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 39,
                "col": 8
              },
              "end": {
                "line": 39,
                "col": 11
              }
            }
          }
        ],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 37,
            "col": 6
          },
          "end": {
            "line": 37,
            "col": 7
          }
        }
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
        "deprecated": false,
        "selection": {
          "start": {
            "line": 35,
            "col": 4
          },
          "end": {
            "line": 35,
            "col": 6
          }
        }
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
        "deprecated": false,
        "selection": {
          "start": {
            "line": 33,
            "col": 11
          },
          "end": {
            "line": 33,
            "col": 13
          }
        }
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
        "deprecated": false,
        "selection": {
          "start": {
            "line": 31,
            "col": 4
          },
          "end": {
            "line": 31,
            "col": 5
          }
        }
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
        "deprecated": false,
        "selection": {
          "start": {
            "line": 29,
            "col": 4
          },
          "end": {
            "line": 29,
            "col": 5
          }
        }
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
        "deprecated": false,
        "selection": {
          "start": {
            "line": 27,
            "col": 6
          },
          "end": {
            "line": 27,
            "col": 7
          }
        }
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
            "deprecated": false,
            "selection": {
              "start": {
                "line": 25,
                "col": 34
              },
              "end": {
                "line": 25,
                "col": 35
              }
            }
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
            "deprecated": false,
            "selection": {
              "start": {
                "line": 25,
                "col": 26
              },
              "end": {
                "line": 25,
                "col": 27
              }
            }
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
            "deprecated": false,
            "selection": {
              "start": {
                "line": 25,
                "col": 18
              },
              "end": {
                "line": 25,
                "col": 19
              }
            }
          }
        ],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 25,
            "col": 8
          },
          "end": {
            "line": 25,
            "col": 13
          }
        }
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
            "deprecated": false,
            "selection": {
              "start": {
                "line": 23,
                "col": 38
              },
              "end": {
                "line": 23,
                "col": 44
              }
            }
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
            "deprecated": false,
            "selection": {
              "start": {
                "line": 23,
                "col": 24
              },
              "end": {
                "line": 23,
                "col": 29
              }
            }
          }
        ],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 23,
            "col": 14
          },
          "end": {
            "line": 23,
            "col": 21
          }
        }
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
        "deprecated": false,
        "selection": {
          "start": {
            "line": 21,
            "col": 10
          },
          "end": {
            "line": 21,
            "col": 12
          }
        }
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
              "col": 4
            },
            "end": {
              "line": 18,
              "col": 22
            },
            "name": "b",
            "kind": "Method",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 18,
                "col": 11
              },
              "end": {
                "line": 18,
                "col": 12
              }
            }
          }
        ],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 16,
            "col": 6
          },
          "end": {
            "line": 16,
            "col": 13
          }
        }
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
            "deprecated": false,
            "selection": {
              "start": {
                "line": 13,
                "col": 2
              },
              "end": {
                "line": 13,
                "col": 23
              }
            }
          }
        ],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 12,
            "col": 11
          },
          "end": {
            "line": 12,
            "col": 23
          }
        }
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
            "deprecated": false,
            "selection": {
              "start": {
                "line": 9,
                "col": 13
              },
              "end": {
                "line": 9,
                "col": 14
              }
            }
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
                "deprecated": false,
                "selection": {
                  "start": {
                    "line": 6,
                    "col": 8
                  },
                  "end": {
                    "line": 6,
                    "col": 11
                  }
                }
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
                "deprecated": false,
                "selection": {
                  "start": {
                    "line": 4,
                    "col": 9
                  },
                  "end": {
                    "line": 4,
                    "col": 10
                  }
                }
              }
            ],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 3,
                "col": 14
              },
              "end": {
                "line": 3,
                "col": 16
              }
            }
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
            "deprecated": false,
            "selection": {
              "start": {
                "line": 2,
                "col": 7
              },
              "end": {
                "line": 2,
                "col": 8
              }
            }
          }
        ],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 1,
            "col": 7
          },
          "end": {
            "line": 1,
            "col": 10
          }
        }
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
  $ $MERLIN single outline -filename foo.mli < foo.mli
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 56,
          "col": 0
        },
        "end": {
          "line": 56,
          "col": 29
        },
        "name": "final_let",
        "kind": "Value",
        "type": "< foo : int >",
        "children": [],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 56,
            "col": 4
          },
          "end": {
            "line": 56,
            "col": 13
          }
        }
      },
      {
        "start": {
          "line": 54,
          "col": 0
        },
        "end": {
          "line": 54,
          "col": 19
        },
        "name": "tb",
        "kind": "ClassType",
        "type": null,
        "children": [],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 54,
            "col": 4
          },
          "end": {
            "line": 54,
            "col": 6
          }
        }
      },
      {
        "start": {
          "line": 50,
          "col": 0
        },
        "end": {
          "line": 52,
          "col": 3
        },
        "name": "ta",
        "kind": "ClassType",
        "type": null,
        "children": [
          {
            "start": {
              "line": 51,
              "col": 2
            },
            "end": {
              "line": 51,
              "col": 35
            },
            "name": "baz",
            "kind": "Method",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 51,
                "col": 2
              },
              "end": {
                "line": 51,
                "col": 35
              }
            }
          }
        ],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 50,
            "col": 11
          },
          "end": {
            "line": 50,
            "col": 13
          }
        }
      },
      {
        "start": {
          "line": 32,
          "col": 0
        },
        "end": {
          "line": 32,
          "col": 19
        },
        "name": "tb",
        "kind": "ClassType",
        "type": null,
        "children": [],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 32,
            "col": 4
          },
          "end": {
            "line": 32,
            "col": 6
          }
        }
      },
      {
        "start": {
          "line": 30,
          "col": 0
        },
        "end": {
          "line": 30,
          "col": 26
        },
        "name": "ta",
        "kind": "ClassType",
        "type": null,
        "children": [],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 30,
            "col": 11
          },
          "end": {
            "line": 30,
            "col": 13
          }
        }
      },
      {
        "start": {
          "line": 22,
          "col": 0
        },
        "end": {
          "line": 22,
          "col": 42
        },
        "name": "point",
        "kind": "Type",
        "type": null,
        "children": [
          {
            "start": {
              "line": 22,
              "col": 34
            },
            "end": {
              "line": 22,
              "col": 40
            },
            "name": "z",
            "kind": "Label",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 22,
                "col": 34
              },
              "end": {
                "line": 22,
                "col": 35
              }
            }
          },
          {
            "start": {
              "line": 22,
              "col": 26
            },
            "end": {
              "line": 22,
              "col": 33
            },
            "name": "y",
            "kind": "Label",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 22,
                "col": 26
              },
              "end": {
                "line": 22,
                "col": 27
              }
            }
          },
          {
            "start": {
              "line": 22,
              "col": 18
            },
            "end": {
              "line": 22,
              "col": 25
            },
            "name": "x",
            "kind": "Label",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 22,
                "col": 18
              },
              "end": {
                "line": 22,
                "col": 19
              }
            }
          }
        ],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 22,
            "col": 8
          },
          "end": {
            "line": 22,
            "col": 13
          }
        }
      },
      {
        "start": {
          "line": 20,
          "col": 0
        },
        "end": {
          "line": 20,
          "col": 50
        },
        "name": "eithery",
        "kind": "Type",
        "type": null,
        "children": [
          {
            "start": {
              "line": 20,
              "col": 36
            },
            "end": {
              "line": 20,
              "col": 50
            },
            "name": "Righty",
            "kind": "Constructor",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 20,
                "col": 38
              },
              "end": {
                "line": 20,
                "col": 44
              }
            }
          },
          {
            "start": {
              "line": 20,
              "col": 24
            },
            "end": {
              "line": 20,
              "col": 35
            },
            "name": "Lefty",
            "kind": "Constructor",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 20,
                "col": 24
              },
              "end": {
                "line": 20,
                "col": 29
              }
            }
          }
        ],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 20,
            "col": 14
          },
          "end": {
            "line": 20,
            "col": 21
          }
        }
      },
      {
        "start": {
          "line": 18,
          "col": 0
        },
        "end": {
          "line": 18,
          "col": 20
        },
        "name": "Ex",
        "kind": "Exn",
        "type": null,
        "children": [],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 18,
            "col": 10
          },
          "end": {
            "line": 18,
            "col": 12
          }
        }
      },
      {
        "start": {
          "line": 10,
          "col": 0
        },
        "end": {
          "line": 12,
          "col": 3
        },
        "name": "class_type_a",
        "kind": "ClassType",
        "type": null,
        "children": [
          {
            "start": {
              "line": 11,
              "col": 2
            },
            "end": {
              "line": 11,
              "col": 23
            },
            "name": "a",
            "kind": "Method",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 11,
                "col": 2
              },
              "end": {
                "line": 11,
                "col": 23
              }
            }
          }
        ],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 10,
            "col": 11
          },
          "end": {
            "line": 10,
            "col": 23
          }
        }
      },
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 8,
          "col": 3
        },
        "name": "Bar",
        "kind": "Module",
        "type": null,
        "children": [
          {
            "start": {
              "line": 4,
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
                "deprecated": false,
                "selection": {
                  "start": {
                    "line": 6,
                    "col": 8
                  },
                  "end": {
                    "line": 6,
                    "col": 11
                  }
                }
              },
              {
                "start": {
                  "line": 5,
                  "col": 4
                },
                "end": {
                  "line": 5,
                  "col": 10
                },
                "name": "t",
                "kind": "Type",
                "type": null,
                "children": [],
                "deprecated": false,
                "selection": {
                  "start": {
                    "line": 5,
                    "col": 9
                  },
                  "end": {
                    "line": 5,
                    "col": 10
                  }
                }
              }
            ],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 4,
                "col": 14
              },
              "end": {
                "line": 4,
                "col": 16
              }
            }
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
            "deprecated": false,
            "selection": {
              "start": {
                "line": 2,
                "col": 7
              },
              "end": {
                "line": 2,
                "col": 8
              }
            }
          }
        ],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 1,
            "col": 7
          },
          "end": {
            "line": 1,
            "col": 10
          }
        }
      }
    ],
    "notifications": []
  }

  $ $MERLIN single outline -filename path.mli < path.mli
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 5,
          "col": 0
        },
        "end": {
          "line": 5,
          "col": 11
        },
        "name": "x",
        "kind": "Value",
        "type": "A.a",
        "children": [],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 5,
            "col": 4
          },
          "end": {
            "line": 5,
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
          "line": 3,
          "col": 3
        },
        "name": "A",
        "kind": "Module",
        "type": null,
        "children": [
          {
            "start": {
              "line": 2,
              "col": 2
            },
            "end": {
              "line": 2,
              "col": 14
            },
            "name": "a",
            "kind": "Type",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 2,
                "col": 7
              },
              "end": {
                "line": 2,
                "col": 8
              }
            }
          }
        ],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 1,
            "col": 7
          },
          "end": {
            "line": 1,
            "col": 8
          }
        }
      }
    ],
    "notifications": []
  }
