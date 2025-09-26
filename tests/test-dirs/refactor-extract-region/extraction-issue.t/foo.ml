let z = 100

let complicated_function x y =
  let a = 10 in
  let b = 11 in
  let c = 12 in
  let module D = struct
    let x = 13
  end in
  a + b + (c * x * y) + z + D.x

let f () =
  let module D = struct
    let x = 42
  end in
  let module M = struct
    let x = 1
  end in
  let a, b, c, x = (1, 2, 3, 4) in
  a + b + c + D.x + x + M.(x + a)
