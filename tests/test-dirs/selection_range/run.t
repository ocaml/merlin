  $ alias enc="$MERLIN single enclosing -position "

  $ cat > main.ml << EOF
  > let i = 
  >   let x = 5 in 
  >   x
  > EOF

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
          "col": 11
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
