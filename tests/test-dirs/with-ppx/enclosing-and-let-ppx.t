  $ cat >dune-project <<EOF
  > (lang dune 2.0)
  > EOF

  $ cat >main.ml <<EOF
  > module Let_syntax = struct
  >   let map x ~f = f x
  >   let both x y = x,y
  > end
  > 
  > let _ =
  >   let%map () = () in ()
  > 
  > let _ =
  >   let%map () = () and () = () in ()
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (preprocess (pps ppx_let)))
  > EOF

  $ dune build @check

  $ $MERLIN single enclosing -position 7:16 -end-position 7:16  -filename main.ml <main.ml | jq .value | extract_ranges main.ml
  ---------- Range 0 ----------
              ···()···
  ---------- Range 1 ----------
  ··let%map () = () in···
  ---------- Range 2 ----------
  ··let%map () = () in ()···
  ---------- Range 3 ----------
  let _ =
    let%map () = () in ()···
  ---------- Range 4 ----------
  module Let_syntax = struct
    let map x ~f = f x
    let both x y = x,y
  end
  
  let _ =
    let%map () = () in ()
  
  let _ =
    let%map () = () and () = () in ()···

FIXME: [merlin.loc_stack] attributes are not stripped from the parsetree that
[expand-ppx] prints, so every parenthesised expression, pattern and type shows
up annotated in the expansion:

  $ cat >main.ml <<EOF
  > module Let_syntax = struct
  >   let map x ~f = f x
  >   let both x y = x,y
  > end
  > 
  > let _ =
  >   let%map (x) = (5) in (x)
  > EOF

  $ dune build @check

  $ LOC=7:7
  $ show_location main.ml $LOC
  module Let_syntax = struct
    let map x ~f = f x
    let both x y = x,y
  end
  
  let _ =
    let%m█p (x) = (5) in (x)
  $ $MERLIN single expand-ppx -position $LOC -filename main.ml <main.ml
  {
    "class": "return",
    "value": {
      "code": "Let_syntax.map ((5)[@merlin.loc_stack ])
    ~f:(fun ((x)[@merlin.loc_stack ]) -> ((x)
          [@merlin.loc ][@merlin.loc_stack ]))",
      "deriver": {
        "start": {
          "line": 7,
          "col": 2
        },
        "end": {
          "line": 7,
          "col": 26
        }
      }
    },
    "notifications": []
  }
