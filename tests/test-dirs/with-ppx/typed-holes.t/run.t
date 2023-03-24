The ppx works as expected without any typed-hole:
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

and with type-holes (since #1503)
  $ cat >main.ml <<EOF
  > match Some 3 with
  > | None -> _
  > | Some _ -> print_int [%get_int 42]
  > EOF

  $ $MERLIN single errors \
  > -filename main.ml < main.ml 
  {
    "class": "return",
    "value": [],
    "notifications": []
  }
