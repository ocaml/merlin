The server might already be running, we kill it to make sure we start from a
clean slate:

  $ (killall ocamlmerlin_server.exe; true) &> /dev/null

First try: nothing has been built:

  $ $MERLIN server errors -filename test.ml < test.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 8
        },
        "end": {
          "line": 1,
          "col": 22
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound module Dep"
      }
    ],
    "notifications": []
  }

Then, we build the dep:

  $ $OCAMLC -c dep.ml

And try again:

  $ $MERLIN server errors -filename test.ml < test.ml
  {
    "class": "return",
    "value": [],
    "notifications": []
  }

If we remove the dep and try again, the "Unbound module" error should reappear:

  $ rm dep.cm*
  $ $MERLIN server errors -filename test.ml < test.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 8
        },
        "end": {
          "line": 1,
          "col": 22
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound module Dep"
      }
    ],
    "notifications": []
  }
