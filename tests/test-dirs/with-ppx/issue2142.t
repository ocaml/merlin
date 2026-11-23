  $ cat >dune-project <<EOF
  > (lang dune 2.0)
  > EOF

  $ cat >main.ml <<EOF
  > module Let_syntax = struct
  >   let map x ~f = f x
  > end
  > 
  > let _ =
  >   let%map (x) = (5) in (x)
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (preprocess (pps ppx_let)))
  > EOF

  $ dune build @check

  $ LOC=6:11
  $ show_location main.ml $LOC
  module Let_syntax = struct
    let map x ~f = f x
  end
  
  let _ =
    let%map (█) = (5) in (x)

  $ $MERLIN single expand-prefix -prefix x -position $LOC -filename main.ml <main.ml
  {
    "class": "return",
    "value": {
      "entries": [],
      "context": null
    },
    "notifications": []
  }
