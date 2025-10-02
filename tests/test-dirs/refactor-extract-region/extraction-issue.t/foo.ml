let z = 100

let complicated_function x y =
  let module D = struct
    let x = 13
  end in
  (x * y) + D.x

let f () =
  let module D = struct
    let x = 42
  end in
  let module M = struct
    let x = 1
  end in
  let x = 10 in
  D.x + x + M.x
