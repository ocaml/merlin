module T = struct
  type t = X of int
end

module Y = struct
  let y = T.X 1
end

let z = Y.y

let z2 = B.x
