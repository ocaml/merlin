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
