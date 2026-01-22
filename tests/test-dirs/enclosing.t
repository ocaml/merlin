FIXME: with 5.2 new function representation we lost some granularity
  $ cat >main.ml <<EOF
  > module M = struct
  >  let g =
  >    let f x = fun y -> Int.add x y in
  >    f 4 5
  > end
  > EOF

  $ $MERLIN single enclosing -position 3:32 -filename main.ml <main.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 32
        },
        "end": {
          "line": 3,
          "col": 33
        }
      },
      {
        "start": {
          "line": 3,
          "col": 22
        },
        "end": {
          "line": 3,
          "col": 33
        }
      },
      {
        "start": {
          "line": 3,
          "col": 13
        },
        "end": {
          "line": 3,
          "col": 33
        }
      },
      {
        "start": {
          "line": 3,
          "col": 9
        },
        "end": {
          "line": 3,
          "col": 33
        }
      },
      {
        "start": {
          "line": 3,
          "col": 3
        },
        "end": {
          "line": 3,
          "col": 33
        }
      },
      {
        "start": {
          "line": 3,
          "col": 3
        },
        "end": {
          "line": 4,
          "col": 8
        }
      },
      {
        "start": {
          "line": 2,
          "col": 1
        },
        "end": {
          "line": 4,
          "col": 8
        }
      },
      {
        "start": {
          "line": 1,
          "col": 11
        },
        "end": {
          "line": 5,
          "col": 3
        }
      },
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 5,
          "col": 3
        }
      }
    ],
    "notifications": []
  }


  $ cat >main.ml <<EOF
  > let f x = x + (succ 1 + 3) + 10
  > EOF

  $ $MERLIN single enclosing -position 1:11 -filename main.ml <main.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 10
        },
        "end": {
          "line": 1,
          "col": 11
        }
      },
      {
        "start": {
          "line": 1,
          "col": 10
        },
        "end": {
          "line": 1,
          "col": 26
        }
      },
      {
        "start": {
          "line": 1,
          "col": 10
        },
        "end": {
          "line": 1,
          "col": 31
        }
      },
      {
        "start": {
          "line": 1,
          "col": 6
        },
        "end": {
          "line": 1,
          "col": 31
        }
      },
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 1,
          "col": 31
        }
      }
    ],
    "notifications": []
  }


  $ cat >main.ml <<EOF
  > let f x = x + (succ 1 + 3) + 10
  > EOF

  $ $MERLIN single enclosing -position 1:11 -stop 1:31  -filename main.ml <main.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 10
        },
        "end": {
          "line": 1,
          "col": 31
        }
      },
      {
        "start": {
          "line": 1,
          "col": 6
        },
        "end": {
          "line": 1,
          "col": 31
        }
      },
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 1,
          "col": 31
        }
      }
    ],
    "notifications": []
  }

  $ cat >main.ml <<EOF
  > let f x = x + (succ 1 + 3) + 10
  > EOF

  $ $MERLIN single enclosing -position 1:15 -stop 1:26  -filename main.ml <main.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 14
        },
        "end": {
          "line": 1,
          "col": 26
        }
      },
      {
        "start": {
          "line": 1,
          "col": 10
        },
        "end": {
          "line": 1,
          "col": 26
        }
      },
      {
        "start": {
          "line": 1,
          "col": 10
        },
        "end": {
          "line": 1,
          "col": 31
        }
      },
      {
        "start": {
          "line": 1,
          "col": 6
        },
        "end": {
          "line": 1,
          "col": 31
        }
      },
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 1,
          "col": 31
        }
      }
    ],
    "notifications": []
  }
