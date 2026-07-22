FIXME: with 5.2 new function representation we lost some granularity
  $ cat >main.ml <<EOF
  > module M = struct
  >  let g =
  >    let f x = fun y -> Int.add x y in
  >    f 4 5
  > end
  > EOF

  $ $MERLIN single enclosing -position 3:32 -filename main.ml <main.ml | jq .value | extract_ranges main.ml
  ---------- Range 0 ----------
                               ···y···
  ---------- Range 1 ----------
                     ···Int.add x y···
  ---------- Range 2 ----------
            ···fun y -> Int.add x y···
  ---------- Range 3 ----------
        ···x = fun y -> Int.add x y···
  ---------- Range 4 ----------
  ···let f x = fun y -> Int.add x y in···
  ---------- Range 5 ----------
  ···let f x = fun y -> Int.add x y in
     f 4 5···
  ---------- Range 6 ----------
  ·let g =
     let f x = fun y -> Int.add x y in
     f 4 5···
  ---------- Range 7 ----------
          ···struct
   let g =
     let f x = fun y -> Int.add x y in
     f 4 5
  end···
  ---------- Range 8 ----------
  module M = struct
   let g =
     let f x = fun y -> Int.add x y in
     f 4 5
  end···


  $ cat >main.ml <<EOF
  > let f x = x + (succ 1 + 3) + 10
  > EOF

  $ $MERLIN single enclosing -position 1:11 -filename main.ml <main.ml | jq .value | extract_ranges main.ml
  ---------- Range 0 ----------
         ···x···
  ---------- Range 1 ----------
         ···x + (succ 1 + 3)···
  ---------- Range 2 ----------
         ···x + (succ 1 + 3) + 10···
  ---------- Range 3 ----------
     ···x = x + (succ 1 + 3) + 10···
  ---------- Range 4 ----------
  let f x = x + (succ 1 + 3) + 10···


  $ cat >main.ml <<EOF
  > let f x = x + (succ 1 + 3) + 10
  > EOF

  $ $MERLIN single enclosing -position 1:11 -end-position 1:31  -filename main.ml <main.ml | jq .value | extract_ranges main.ml
  ---------- Range 0 ----------
         ···x + (succ 1 + 3) + 10···
  ---------- Range 1 ----------
     ···x = x + (succ 1 + 3) + 10···
  ---------- Range 2 ----------
  let f x = x + (succ 1 + 3) + 10···

  $ cat >main.ml <<EOF
  > let f x = x + (succ 1 + 3) + 10
  > EOF

  $ $MERLIN single enclosing -position 1:15 -end-position 1:26  -filename main.ml <main.ml | jq .value | extract_ranges main.ml
  ---------- Range 0 ----------
             ···(succ 1 + 3)···
  ---------- Range 1 ----------
         ···x + (succ 1 + 3)···
  ---------- Range 2 ----------
         ···x + (succ 1 + 3) + 10···
  ---------- Range 3 ----------
     ···x = x + (succ 1 + 3) + 10···
  ---------- Range 4 ----------
  let f x = x + (succ 1 + 3) + 10···

  $ cat >main.ml <<EOF
  > let () =
  >   let x = () in
  >   ()
  > EOF

  $ $MERLIN single enclosing -position 2:11 -end-position 2:11  -filename main.ml <main.ml | jq .value | extract_ranges main.ml
  ---------- Range 0 ----------
         ···()···
  ---------- Range 1 ----------
  ··let x = () in···
  ---------- Range 2 ----------
  ··let x = () in
    ()···
  ---------- Range 3 ----------
  let () =
    let x = () in
    ()···

  $ cat >main.ml <<EOF
  > let () =
  >   let x1 = ()
  >   and x2 = () in
  >   ()
  > EOF

  $ $MERLIN single enclosing -position 3:12 -end-position 3:12  -filename main.ml <main.ml | jq .value | extract_ranges main.ml
  ---------- Range 0 ----------
          ···()···
  ---------- Range 1 ----------
  ··let x1 = ()
    and x2 = () in···
  ---------- Range 2 ----------
  ··let x1 = ()
    and x2 = () in
    ()···
  ---------- Range 3 ----------
  let () =
    let x1 = ()
    and x2 = () in
    ()···


  $ cat >main.ml <<EOF
  > let () =
  >   let x1 = () in
  >   let x2 = () in
  >   let x3 = () in
  >   let x4 = () in
  >   let x5 = () in
  >   ()
  > EOF

  $ $MERLIN single enclosing -position 4:11 -end-position 4:11  -filename main.ml <main.ml | jq .value | extract_ranges main.ml
  ---------- Range 0 ----------
          ···()···
  ---------- Range 1 ----------
  ··let x3 = () in···
  ---------- Range 2 ----------
  ··let x3 = () in
    let x4 = () in···
  ---------- Range 3 ----------
  ··let x3 = () in
    let x4 = () in
    let x5 = () in···
  ---------- Range 4 ----------
  ··let x3 = () in
    let x4 = () in
    let x5 = () in
    ()···
  ---------- Range 5 ----------
  ··let x2 = () in
    let x3 = () in
    let x4 = () in
    let x5 = () in
    ()···
  ---------- Range 6 ----------
  ··let x1 = () in
    let x2 = () in
    let x3 = () in
    let x4 = () in
    let x5 = () in
    ()···
  ---------- Range 7 ----------
  let () =
    let x1 = () in
    let x2 = () in
    let x3 = () in
    let x4 = () in
    let x5 = () in
    ()···

  $ cat >main.ml <<EOF
  > let () =
  >   ();
  >   ();
  >   ();
  >   ();
  >   ()
  > EOF

  $ $MERLIN single enclosing -position 4:3 -end-position 4:3  -filename main.ml <main.ml | jq .value | extract_ranges main.ml
  ---------- Range 0 ----------
  ··()···
  ---------- Range 1 ----------
  ··();
    ()···
  ---------- Range 2 ----------
  ··();
    ();
    ()···
  ---------- Range 3 ----------
  ··();
    ();
    ();
    ()···
  ---------- Range 4 ----------
  ··();
    ();
    ();
    ();
    ()···
  ---------- Range 5 ----------
  let () =
    ();
    ();
    ();
    ();
    ()···

  $ cat >main.ml <<EOF
  > let (let>) x f = f x
  > let () =
  >   let> x = () in
  >   ()
  > EOF

  $ $MERLIN single enclosing -position 3:12 -end-position 3:12  -filename main.ml <main.ml | jq .value | extract_ranges main.ml
  ---------- Range 0 ----------
          ···()···
  ---------- Range 1 ----------
  ··let> x = () in
    ()···
  ---------- Range 2 ----------
  let () =
    let> x = () in
    ()···
  ---------- Range 3 ----------
  let (let>) x f = f x
  let () =
    let> x = () in
    ()···

  $ cat >main.ml <<EOF
  > let () = ()
  > 
  > let%test () =
  >   ()
  > 
  > let () = ()
  > EOF

  $ $MERLIN single enclosing -position 4:3 -end-position 4:3  -filename main.ml <main.ml | jq .value | extract_ranges main.ml
  ---------- Range 0 ----------
  let () = ()
  
  let%test () =
    ()
  
  let () = ()···
