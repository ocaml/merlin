let i =
  let x = 5 in
  let y = 6 in
  x

let () =
  ();
  ();
  ();
  ();
  ()

let ( let+ ) x f = f x
let ( and+ ) x y = (x, y)
let _ =
  let+ x = 5 and+ y = 6 in
  x + y
