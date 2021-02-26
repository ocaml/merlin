module T = struct
  type t = { foobar : int; test_other : float; }
end

let _easy = { T.f }

let _hard = ({ foo } : T.t)

open T

let _easier = { foobar = 5; tes }
