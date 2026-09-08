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

In range mode the expansion starts from the requested selection, so every
range returned contains it:

  $ cat >main.ml <<EOF
  > let f () =
  >   print_int 1;
  >   print_int 2;
  >   print_int 3
  > EOF

  $ $MERLIN single enclosing -position 2:2 -end-position 4:13 -filename main.ml <main.ml | jq .value | extract_ranges main.ml
  ---------- Range 0 ----------
     ···() =
    print_int 1;
    print_int 2;
    print_int 3···
  ---------- Range 1 ----------
  let f () =
    print_int 1;
    print_int 2;
    print_int 3···

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
  >   and x2 = 43 in
  >   ()
  > EOF

  $ $MERLIN single enclosing -position 2:12 -end-position 2:12  -filename main.ml <main.ml | jq .value | extract_ranges main.ml
  ---------- Range 0 ----------
          ···()···
  ---------- Range 1 ----------
  ··let x1 = ()
    and x2 = 43 in···
  ---------- Range 2 ----------
  ··let x1 = ()
    and x2 = 43 in
    ()···
  ---------- Range 3 ----------
  let () =
    let x1 = ()
    and x2 = 43 in
    ()···

  $ $MERLIN single enclosing -position 3:12 -end-position 3:12  -filename main.ml <main.ml | jq .value | extract_ranges main.ml
  ---------- Range 0 ----------
          ···43···
  ---------- Range 1 ----------
  ··let x1 = ()
    and x2 = 43 in···
  ---------- Range 2 ----------
  ··let x1 = ()
    and x2 = 43 in
    ()···
  ---------- Range 3 ----------
  let () =
    let x1 = ()
    and x2 = 43 in
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
  >   (); (* 1 *)
  >   (); (* 2 *)
  >   (); (* 3 *)
  >   (); (* 4 *)
  >   ()  (* 5 *)
  > EOF

  $ $MERLIN single enclosing -position 4:3 -end-position 4:3  -filename main.ml <main.ml | jq .value | extract_ranges main.ml
  ---------- Range 0 ----------
  ··()···
  ---------- Range 1 ----------
  ··();···
  ---------- Range 2 ----------
  ··(); (* 3 *)
    ();···
  ---------- Range 3 ----------
  ··(); (* 3 *)
    (); (* 4 *)
    ()···
  ---------- Range 4 ----------
  ··(); (* 2 *)
    (); (* 3 *)
    (); (* 4 *)
    ()···
  ---------- Range 5 ----------
  ··(); (* 1 *)
    (); (* 2 *)
    (); (* 3 *)
    (); (* 4 *)
    ()···
  ---------- Range 6 ----------
  let () =
    (); (* 1 *)
    (); (* 2 *)
    (); (* 3 *)
    (); (* 4 *)
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
    ···
  ---------- Range 2 ----------
  ··let> x = () in
    ()···
  ---------- Range 3 ----------
  let () =
    let> x = () in
    ()···
  ---------- Range 4 ----------
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


  $ cat >main.ml <<EOF
  > let () =
  >   let () = () in
  >   ()
  > EOF

  $ $MERLIN single enclosing -position 2:12 -end-position 2:12  -filename main.ml <main.ml | jq .value | extract_ranges main.ml
  ---------- Range 0 ----------
          ···()···
  ---------- Range 1 ----------
  ··let () = () in···
  ---------- Range 2 ----------
  ··let () = () in
    ()···
  ---------- Range 3 ----------
  let () =
    let () = () in
    ()···

  $ cat >main.ml <<EOF
  > let (let+) x f = f x
  > let (and+) x y = (x, y)
  > let _ =
  >   let+ a = 5 in
  >   let+ b = 5 in
  >   let+ c = 5 in
  >   let+ x = 5
  >   and+ y = 6 in
  >   let+ d = 5 in
  >   let+ e = 5 in
  >   x + y
  > EOF

  $ $MERLIN single enclosing -position 6:8 -end-position 6:8  -filename main.ml <main.ml | jq .value | extract_ranges main.ml
  ---------- Range 0 ----------
      ···c···
  ---------- Range 1 ----------
  ··let+ c = 5 in
    ···
  ---------- Range 2 ----------
  ··let+ c = 5 in
    let+ x = 5
    and+ y = 6 in
    ···
  ---------- Range 3 ----------
  ··let+ c = 5 in
    let+ x = 5
    and+ y = 6 in
    let+ d = 5 in
    ···
  ---------- Range 4 ----------
  ··let+ c = 5 in
    let+ x = 5
    and+ y = 6 in
    let+ d = 5 in
    let+ e = 5 in
    ···
  ---------- Range 5 ----------
  ··let+ c = 5 in
    let+ x = 5
    and+ y = 6 in
    let+ d = 5 in
    let+ e = 5 in
    x + y···
  ---------- Range 6 ----------
  ··let+ b = 5 in
    let+ c = 5 in
    let+ x = 5
    and+ y = 6 in
    let+ d = 5 in
    let+ e = 5 in
    x + y···
  ---------- Range 7 ----------
  ··let+ a = 5 in
    let+ b = 5 in
    let+ c = 5 in
    let+ x = 5
    and+ y = 6 in
    let+ d = 5 in
    let+ e = 5 in
    x + y···
  ---------- Range 8 ----------
  let _ =
    let+ a = 5 in
    let+ b = 5 in
    let+ c = 5 in
    let+ x = 5
    and+ y = 6 in
    let+ d = 5 in
    let+ e = 5 in
    x + y···
  ---------- Range 9 ----------
  let (let+) x f = f x
  let (and+) x y = (x, y)
  let _ =
    let+ a = 5 in
    let+ b = 5 in
    let+ c = 5 in
    let+ x = 5
    and+ y = 6 in
    let+ d = 5 in
    let+ e = 5 in
    x + y···

Constraints!

  $ cat >main.ml <<EOF
  > let f () =
  >   let x = 1 in
  >   (x : int)
  > EOF

  $ $MERLIN single enclosing -position 2:6 -end-position 2:6 -filename main.ml <main.ml | jq .value | extract_ranges main.ml
  ---------- Range 0 ----------
     ···x···
  ---------- Range 1 ----------
  ··let x = 1 in
    ···
  ---------- Range 2 ----------
  ··let x = 1 in
    (x : int)···
  ---------- Range 3 ----------
     ···() =
    let x = 1 in
    (x : int)···
  ---------- Range 4 ----------
  let f () =
    let x = 1 in
    (x : int)···

  $ $MERLIN single enclosing -position 3:4 -end-position 3:4 -filename main.ml <main.ml | jq .value | extract_ranges main.ml
  ---------- Range 0 ----------
  ···x···
  ---------- Range 1 ----------
  ··(x : int)···
  ---------- Range 2 ----------
  ··let x = 1 in
    (x : int)···
  ---------- Range 3 ----------
     ···() =
    let x = 1 in
    (x : int)···
  ---------- Range 4 ----------
  let f () =
    let x = 1 in
    (x : int)···


  $ cat >main.ml <<EOF
  > let f () =
  >   (((x : int) : int) : int)
  > EOF

  $ $MERLIN single enclosing -position 2:6 -end-position 2:6 -filename main.ml <main.ml | jq .value | extract_ranges main.ml
  ---------- Range 0 ----------
    ···x···
  ---------- Range 1 ----------
   ···(x : int)···
  ---------- Range 2 ----------
  ···((x : int) : int)···
  ---------- Range 3 ----------
  ··(((x : int) : int) : int)···
  ---------- Range 4 ----------
     ···() =
    (((x : int) : int) : int)···
  ---------- Range 5 ----------
  let f () =
    (((x : int) : int) : int)···

Parentheses are not nodes of their own: the parser relocates the expression
they enclose. The [merlin.loc_stack] attributes mirroring [pexp_loc_stack] let
each level of parenthesising be an expansion step of its own.

  $ cat >main.ml <<EOF
  > let f x = ((x)) + 1
  > EOF

  $ $MERLIN single enclosing -position 1:12 -filename main.ml <main.ml | jq .value | extract_ranges main.ml
  ---------- Range 0 ----------
           ···x···
  ---------- Range 1 ----------
          ···(x)···
  ---------- Range 2 ----------
         ···((x))···
  ---------- Range 3 ----------
         ···((x)) + 1···
  ---------- Range 4 ----------
     ···x = ((x)) + 1···
  ---------- Range 5 ----------
  let f x = ((x)) + 1···

Also when the parenthesised expression is not a leaf:

  $ cat >main.ml <<EOF
  > let f x =
  >   (x + 1) * 2
  > EOF

  $ $MERLIN single enclosing -position 2:3 -filename main.ml <main.ml | jq .value | extract_ranges main.ml
  ---------- Range 0 ----------
  ···x···
  ---------- Range 1 ----------
  ···x + 1···
  ---------- Range 2 ----------
  ··(x + 1)···
  ---------- Range 3 ----------
  ··(x + 1) * 2···
  ---------- Range 4 ----------
     ···x =
    (x + 1) * 2···
  ---------- Range 5 ----------
  let f x =
    (x + 1) * 2···

[begin ... end] does not go through [reloc_exp] -- it rebuilds the node to
carry its extension and attributes, dropping everything else -- so the parser
pushes the location explicitly there:

  $ cat >main.ml <<EOF
  > let f x =
  >   begin x end + 1
  > EOF

  $ $MERLIN single enclosing -position 2:8 -filename main.ml <main.ml | jq .value | extract_ranges main.ml
  ---------- Range 0 ----------
       ···x···
  ---------- Range 1 ----------
  ··begin x end···
  ---------- Range 2 ----------
  ··begin x end + 1···
  ---------- Range 3 ----------
     ···x =
    begin x end + 1···
  ---------- Range 4 ----------
  let f x =
    begin x end + 1···

Both delimiters nest, in either order:

  $ cat >main.ml <<EOF
  > let f x =
  >   (begin (x) end) + 1
  > EOF

  $ $MERLIN single enclosing -position 2:10 -filename main.ml <main.ml | jq .value | extract_ranges main.ml
  ---------- Range 0 ----------
         ···x···
  ---------- Range 1 ----------
        ···(x)···
  ---------- Range 2 ----------
  ···begin (x) end···
  ---------- Range 3 ----------
  ··(begin (x) end)···
  ---------- Range 4 ----------
  ··(begin (x) end) + 1···
  ---------- Range 5 ----------
     ···x =
    (begin (x) end) + 1···
  ---------- Range 6 ----------
  let f x =
    (begin (x) end) + 1···

When a node is under parenthesis or begin ... end, we should not go into it. Otherwise only the first delimiter is included.

  $ cat >main.ml <<EOF
  > let () =
  >   (); (* 1 *)
  >   (); (* 2 *)
  >   (); (* 3 *)
  >   ( (); (* 4 *)
  >     ()  (* 5 *))
  > EOF

  $ $MERLIN single enclosing -position 4:3 -end-position 4:3  -filename main.ml <main.ml | jq .value | extract_ranges main.ml
  ---------- Range 0 ----------
  ··()···
  ---------- Range 1 ----------
  ··();···
  ---------- Range 2 ----------
  ··(); (* 3 *)
    ( (); (* 4 *)
      ()  (* 5 *))···
  ---------- Range 3 ----------
  ··(); (* 2 *)
    (); (* 3 *)
    ( (); (* 4 *)
      ()  (* 5 *))···
  ---------- Range 4 ----------
  ··(); (* 1 *)
    (); (* 2 *)
    (); (* 3 *)
    ( (); (* 4 *)
      ()  (* 5 *))···
  ---------- Range 5 ----------
  let () =
    (); (* 1 *)
    (); (* 2 *)
    (); (* 3 *)
    ( (); (* 4 *)
      ()  (* 5 *))···

A location of the stack is only usable while it contains the cursor: on a
delimiter it does not, and merging it in would give a range holding that
delimiter without its counterpart. The first step is the delimited group
instead. On the opening one:

  $ cat >main.ml <<EOF
  > let f x = ((x)) + 1
  > EOF

  $ $MERLIN single enclosing -position 1:11 -filename main.ml <main.ml | jq .value | extract_ranges main.ml
  ---------- Range 0 ----------
          ···(x)···
  ---------- Range 1 ----------
         ···((x))···
  ---------- Range 2 ----------
         ···((x)) + 1···
  ---------- Range 3 ----------
     ···x = ((x)) + 1···
  ---------- Range 4 ----------
  let f x = ((x)) + 1···

And on the closing one:

  $ $MERLIN single enclosing -position 1:14 -filename main.ml <main.ml | jq .value | extract_ranges main.ml
  ---------- Range 0 ----------
          ···(x)···
  ---------- Range 1 ----------
         ···((x))···
  ---------- Range 2 ----------
         ···((x)) + 1···
  ---------- Range 3 ----------
     ···x = ((x)) + 1···
  ---------- Range 4 ----------
  let f x = ((x)) + 1···

Patterns get their steps too:

  $ cat >main.ml <<EOF
  > let f ((x)) = x
  > EOF

  $ $MERLIN single enclosing -position 1:9 -filename main.ml <main.ml | jq .value | extract_ranges main.ml
  ---------- Range 0 ----------
       ···x···
  ---------- Range 1 ----------
      ···(x)···
  ---------- Range 2 ----------
     ···((x))···
  ---------- Range 3 ----------
     ···((x)) = x···
  ---------- Range 4 ----------
  let f ((x)) = x···

And so do core types:

  $ cat >main.ml <<EOF
  > let f (x : ((int))) = x
  > EOF

  $ $MERLIN single enclosing -position 1:13 -filename main.ml <main.ml | jq .value | extract_ranges main.ml
  ---------- Range 0 ----------
            ···int···
  ---------- Range 1 ----------
           ···(int)···
  ---------- Range 2 ----------
          ···((int))···
  ---------- Range 3 ----------
     ···(x : ((int))) = x···
  ---------- Range 4 ----------
  let f (x : ((int))) = x···

FIXME: splitting a node that is itself parenthesised uses its relocated
location, so the left part starts at the opening delimiter. With the cursor
inside the group:

  $ cat >main.ml <<EOF
  > let g () = ()
  > let () =
  >   g ();
  >   ( g ();
  >     g ())
  > EOF

  $ $MERLIN single enclosing -position 4:4 -filename main.ml <main.ml | jq .value | extract_ranges main.ml
  ---------- Range 0 ----------
   ···g···
  ---------- Range 1 ----------
   ···g ()···
  ---------- Range 2 ----------
  ··( g ();···
  ---------- Range 3 ----------
  ··( g ();
      g ())···
  ---------- Range 4 ----------
  ··g ();
    ( g ();
      g ())···
  ---------- Range 5 ----------
  let () =
    g ();
    ( g ();
      g ())···
  ---------- Range 6 ----------
  let g () = ()
  let () =
    g ();
    ( g ();
      g ())···
