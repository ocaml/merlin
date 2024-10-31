  $ cat >main.ml <<EOF
  > let f () = List.map (fun _ -> 
  >   ())
  > let f x y z = 
  >   ()
  > let f = fun x -> fun y -> fun z -> 
  >   ()
  > EOF
FIXME: Jump to `fun` should not raise an exception and jump to 1:25
  $ $MERLIN single jump -target fun -position 2:4 \
  > -filename main.ml <main.ml | tr '\n' ' ' | jq '.class'
  "exception"

Shoud jump to line 3
  $ $MERLIN single jump -target fun -position 4:4 \
  > -filename main.ml <main.ml 
  {
    "class": "return",
    "value": {
      "pos": {
        "line": 3,
        "col": 0
      }
    },
    "notifications": []
  }

FIXME shoud jump to line 5
  $ $MERLIN single jump -target fun -position 6:4 \
  > -filename main.ml <main.ml  | tr '\n' ' ' | jq '.class'
  "exception"
