  $ cat >main.ml <<EOF
  > match Some 3 with
  > | None -> ()
  > | Some _ -> print_int [%get_int 42]
  > EOF

  $ dune exec ./main.exe 2>/dev/null
  42

  $ $MERLIN single errors \
  > -filename main.ml < main.ml 
  {
    "class": "return",
    "value": [],
    "notifications": []
  }

  $ cat >main.ml <<EOF
  > match Some 3 with
  > | None -> _
  > | Some _ -> print_int [%get_int 42]
  > EOF

  $ $MERLIN single errors \
  > -filename main.ml < main.ml 
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
          "col": 6
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This match case could not be refuted.
  Here is an example of a value that would reach it: None"
      }
    ],
    "notifications": []
  }
