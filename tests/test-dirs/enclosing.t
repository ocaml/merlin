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
  ···let f x = fun y -> Int.add x y···
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
