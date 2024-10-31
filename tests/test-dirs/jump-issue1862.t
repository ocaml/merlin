  $ cat >main.ml <<EOF
  > let f () = List.map (fun _ -> 
  >   ())
  > let f x y z = 
  >   ()
  > let f = fun x -> fun y -> fun z -> 
  >   ()
  > EOF

Jump to `fun` should not raise an exception and jump to 1:20
  $ $MERLIN single jump -target fun -position 2:4 \
  > -filename main.ml <main.ml | jq '.value.pos'
  {
    "line": 1,
    "col": 20
  }

Shoud jump to line 3
  $ $MERLIN single jump -target fun -position 4:4 \
  > -filename main.ml <main.ml | jq '.value.pos'
  {
    "line": 3,
    "col": 0
  }

Shoud jump to line 5
  $ $MERLIN single jump -target fun -position 6:4 \
  > -filename main.ml <main.ml | jq '.value.pos'
  {
    "line": 5,
    "col": 0
  }
