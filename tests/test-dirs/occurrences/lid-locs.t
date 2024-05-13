An ident that doesn't need parenthesis:
The parenthesis are typed as an open statement

  $ cat >test.ml <<'EOF'
  > module M = struct
  >   let x = 42
  > end
  > let _ = M.x
  > let _ = M. x
  > let _ = M.( x )
  > let _ = M.( ( x ) )
  > let _ = M. 
  >   x
  > EOF

  $ $MERLIN single occurrences -identifier-at 4:10 ./test.ml < ./test.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 6
        },
        "end": {
          "line": 2,
          "col": 7
        }
      },
      {
        "start": {
          "line": 4,
          "col": 10
        },
        "end": {
          "line": 4,
          "col": 11
        }
      },
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
          "line": 6,
          "col": 12
        },
        "end": {
          "line": 6,
          "col": 13
        }
      },
      {
        "start": {
          "line": 7,
          "col": 14
        },
        "end": {
          "line": 7,
          "col": 15
        }
      },
      {
        "start": {
          "line": 9,
          "col": 2
        },
        "end": {
          "line": 9,
          "col": 3
        }
      }
    ],
    "notifications": []
  }

An ident that needs parenthesis:
We don't have enough information to safely shrink the enclosing
  $ cat >test.ml <<'EOF'
  > module M = struct
  >   let (++) a b = a + b
  > end
  > let _ = M.(++)
  > let _ = M.( ++ )
  > let _ = M. ( ++)
  > let _ = M. (
  >   ++)
  > let _ = let open M in ( ++ )
  > EOF

  $ $MERLIN single occurrences -identifier-at 4:12 ./test.ml < ./test.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 6
        },
        "end": {
          "line": 2,
          "col": 10
        }
      },
      {
        "start": {
          "line": 4,
          "col": 8
        },
        "end": {
          "line": 4,
          "col": 14
        }
      },
      {
        "start": {
          "line": 5,
          "col": 8
        },
        "end": {
          "line": 5,
          "col": 16
        }
      },
      {
        "start": {
          "line": 6,
          "col": 8
        },
        "end": {
          "line": 6,
          "col": 16
        }
      },
      {
        "start": {
          "line": 7,
          "col": 8
        },
        "end": {
          "line": 8,
          "col": 5
        }
      },
      {
        "start": {
          "line": 9,
          "col": 22
        },
        "end": {
          "line": 9,
          "col": 28
        }
      }
    ],
    "notifications": []
  }

Same for an ident that requires both parenthesis and spaces:
  $ cat >test.ml <<'EOF'
  > module M = struct
  >   let ( * ) a b = a * b
  > end
  > let _ = M.( * )
  > let _ = M. (  * )
  > let _ = M.( *)
  > let _ = M. (
  >   *)
  > EOF

  $ $MERLIN single occurrences -identifier-at 4:12 ./test.ml < ./test.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 6
        },
        "end": {
          "line": 2,
          "col": 11
        }
      },
      {
        "start": {
          "line": 4,
          "col": 8
        },
        "end": {
          "line": 4,
          "col": 15
        }
      },
      {
        "start": {
          "line": 5,
          "col": 8
        },
        "end": {
          "line": 5,
          "col": 17
        }
      },
      {
        "start": {
          "line": 6,
          "col": 8
        },
        "end": {
          "line": 6,
          "col": 14
        }
      },
      {
        "start": {
          "line": 7,
          "col": 8
        },
        "end": {
          "line": 8,
          "col": 4
        }
      }
    ],
    "notifications": []
  }
