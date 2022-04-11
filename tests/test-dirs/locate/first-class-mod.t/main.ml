module type MT = module type of Lib.Packed_mod
let m = (module Lib.Packed_mod : MT);;

let module M = (val m) in

Printf.printf "%i\n" M.x
