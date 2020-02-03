type t = U
type t' = U

let f : t  = U

let g (x : t) =
  match x with
  | U -> ()

module M = struct
  type t = A
  type u = A | B
end

let f () = (M.A : M.t);
