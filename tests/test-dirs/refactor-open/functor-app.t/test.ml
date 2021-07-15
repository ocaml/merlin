module Wrapper = struct
  module type S = sig
    type t
    val x : t
  end

  module Make (X : S) = struct
    include X

    let () = ignore x
  end

  module C = struct
    type t = char
    let x  = 'a'
  end
end

module I = struct
  type t = int
  let x  = 42
end

module Hammer = Wrapper

open Wrapper

module MC = Hammer (* just because *).Make (Wrapper.C)
module MD = Wrapper.Make (Wrapper.C)

module MI = Wrapper.Make (I)

module MC2 = Make (C)
module MI2 = Make (I)

module MC3 = Make (Wrapper.C)
module MC4 = Wrapper.Make (C)

type t1 = Wrapper.Make(Wrapper.C).t
type t2 = Make(Wrapper.C).t
type t3 = Wrapper.Make(C).t
type t4 = Make(C).t
