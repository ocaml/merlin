  $ alias enc="$MERLIN single enclosing -position "

  $ cat > main.ml << EOF
  > let i =
  >   let x = 5 in
  >   x
  > EOF

  $ export MERLIN_LOG=/tmp/merlin.log
  $ enc 2:5 -filename ./main.ml < ./main.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 2
        },
        "end": {
          "line": 2,
          "col": 14
        }
      },
      {
        "start": {
          "line": 2,
          "col": 2
        },
        "end": {
          "line": 3,
          "col": 3
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
        }
      }
    ],
    "notifications": []
  }


  $ cat > main.ml << EOF
  > let () =
  >   ();
  >   ();
  >   ();
  >   ();
  >   ()
  > EOF

  $ enc 2:2 -filename ./main.ml < ./main.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 2
        },
        "end": {
          "line": 2,
          "col": 4
        }
      },
      {
        "start": {
          "line": 2,
          "col": 2
        },
        "end": {
          "line": 3,
          "col": 2
        }
      },
      {
        "start": {
          "line": 2,
          "col": 2
        },
        "end": {
          "line": 4,
          "col": 2
        }
      },
      {
        "start": {
          "line": 2,
          "col": 2
        },
        "end": {
          "line": 5,
          "col": 2
        }
      },
      {
        "start": {
          "line": 2,
          "col": 2
        },
        "end": {
          "line": 6,
          "col": 2
        }
      },
      {
        "start": {
          "line": 2,
          "col": 2
        },
        "end": {
          "line": 6,
          "col": 4
        }
      },
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 6,
          "col": 4
        }
      }
    ],
    "notifications": []
  }

  $ cat > main.ml << EOF
  > let _ =
  >   let* n = Ok 5 in Ok (n + 10)
  > EOF

  $ enc 2:11 -filename ./main.ml < ./main.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 2
        },
        "end": {
          "line": 2,
          "col": 30
        }
      },
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 2,
          "col": 30
        }
      }
    ],
    "notifications": []
  }

  $ cat > main.ml << EOF
  > let (let+) x f = f x
  > let (and+) x y = (x, y)
  > let _ =
  >   let+ x = 5
  >   and+ y = 6 in
  >   x + y
  > EOF

  $ enc 5:11 -filename ./main.ml < ./main.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 5,
          "col": 11
        },
        "end": {
          "line": 5,
          "col": 12
        }
      },
      {
        "start": {
          "line": 4,
          "col": 2
        },
        "end": {
          "line": 6,
          "col": 2
        }
      },
      {
        "start": {
          "line": 4,
          "col": 2
        },
        "end": {
          "line": 6,
          "col": 7
        }
      },
      {
        "start": {
          "line": 3,
          "col": 0
        },
        "end": {
          "line": 6,
          "col": 7
        }
      },
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 6,
          "col": 7
        }
      }
    ],
    "notifications": []
  }

  $ cat > main.ml << EOF
  > let f x =
  >   let (Some y) = x in
  >   y
  > EOF

  $ enc 2:17 -filename ./main.ml < ./main.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 17
        },
        "end": {
          "line": 2,
          "col": 18
        }
      },
      {
        "start": {
          "line": 2,
          "col": 2
        },
        "end": {
          "line": 2,
          "col": 21
        }
      },
      {
        "start": {
          "line": 2,
          "col": 2
        },
        "end": {
          "line": 3,
          "col": 3
        }
      },
      {
        "start": {
          "line": 1,
          "col": 6
        },
        "end": {
          "line": 3,
          "col": 3
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
        }
      }
    ],
    "notifications": []
  }

  $ cat > main.ml << EOF
  > let (let+) x f = f x
  > let (and+) x y = (x, y)
  > let f x =
  >   let+ a = 5
  >   and+ b = 6 in
  >   let (Some c) = x in
  >   a;
  >   b;
  >   c
  > EOF

  $ enc 4:11 -filename ./main.ml < ./main.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 11
        },
        "end": {
          "line": 4,
          "col": 12
        }
      },
      {
        "start": {
          "line": 4,
          "col": 2
        },
        "end": {
          "line": 6,
          "col": 2
        }
      },
      {
        "start": {
          "line": 4,
          "col": 2
        },
        "end": {
          "line": 6,
          "col": 21
        }
      },
      {
        "start": {
          "line": 4,
          "col": 2
        },
        "end": {
          "line": 8,
          "col": 2
        }
      },
      {
        "start": {
          "line": 4,
          "col": 2
        },
        "end": {
          "line": 9,
          "col": 2
        }
      },
      {
        "start": {
          "line": 4,
          "col": 2
        },
        "end": {
          "line": 9,
          "col": 3
        }
      },
      {
        "start": {
          "line": 3,
          "col": 6
        },
        "end": {
          "line": 9,
          "col": 3
        }
      },
      {
        "start": {
          "line": 3,
          "col": 0
        },
        "end": {
          "line": 9,
          "col": 3
        }
      },
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 9,
          "col": 3
        }
      }
    ],
    "notifications": []
  }
  $ enc 6:17 -filename ./main.ml < ./main.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 6,
          "col": 17
        },
        "end": {
          "line": 6,
          "col": 18
        }
      },
      {
        "start": {
          "line": 6,
          "col": 2
        },
        "end": {
          "line": 6,
          "col": 21
        }
      },
      {
        "start": {
          "line": 6,
          "col": 2
        },
        "end": {
          "line": 8,
          "col": 2
        }
      },
      {
        "start": {
          "line": 6,
          "col": 2
        },
        "end": {
          "line": 9,
          "col": 2
        }
      },
      {
        "start": {
          "line": 6,
          "col": 2
        },
        "end": {
          "line": 9,
          "col": 3
        }
      },
      {
        "start": {
          "line": 4,
          "col": 2
        },
        "end": {
          "line": 9,
          "col": 3
        }
      },
      {
        "start": {
          "line": 3,
          "col": 6
        },
        "end": {
          "line": 9,
          "col": 3
        }
      },
      {
        "start": {
          "line": 3,
          "col": 0
        },
        "end": {
          "line": 9,
          "col": 3
        }
      },
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 9,
          "col": 3
        }
      }
    ],
    "notifications": []
  }
  $ enc 8:2 -filename ./main.ml < ./main.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 8,
          "col": 2
        },
        "end": {
          "line": 8,
          "col": 3
        }
      },
      {
        "start": {
          "line": 8,
          "col": 2
        },
        "end": {
          "line": 9,
          "col": 2
        }
      },
      {
        "start": {
          "line": 8,
          "col": 2
        },
        "end": {
          "line": 9,
          "col": 3
        }
      },
      {
        "start": {
          "line": 7,
          "col": 2
        },
        "end": {
          "line": 9,
          "col": 3
        }
      },
      {
        "start": {
          "line": 6,
          "col": 6
        },
        "end": {
          "line": 9,
          "col": 3
        }
      },
      {
        "start": {
          "line": 6,
          "col": 2
        },
        "end": {
          "line": 9,
          "col": 3
        }
      },
      {
        "start": {
          "line": 4,
          "col": 2
        },
        "end": {
          "line": 9,
          "col": 3
        }
      },
      {
        "start": {
          "line": 3,
          "col": 6
        },
        "end": {
          "line": 9,
          "col": 3
        }
      },
      {
        "start": {
          "line": 3,
          "col": 0
        },
        "end": {
          "line": 9,
          "col": 3
        }
      },
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 9,
          "col": 3
        }
      }
    ],
    "notifications": []
  }
