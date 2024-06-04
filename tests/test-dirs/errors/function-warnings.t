  $ $MERLIN single errors -filename fun_bad.ml <<EOF
  > let f () = ()
  > let x = f () 0
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 8
        },
        "end": {
          "line": 2,
          "col": 14
        },
        "type": "typer",
        "sub": [
          {
            "start": {
              "line": 2,
              "col": 8
            },
            "end": {
              "line": 2,
              "col": 14
            },
            "message": "Hint: Did you forget a ';'?"
          },
          {
            "start": {
              "line": 2,
              "col": 8
            },
            "end": {
              "line": 2,
              "col": 14
            },
            "message": "This extra argument is not expected."
          }
        ],
        "valid": true,
        "message": "The function f has type unit -> unit
  It is applied to too many arguments"
      }
    ],
    "notifications": []
  }
