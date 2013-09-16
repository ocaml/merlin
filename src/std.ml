module List = struct
  include ListLabels

  let rec filter_map ~f = function
    | [] -> []
    | x :: xs ->
      match f x with
      | None -> filter_map ~f xs
      | Some x -> x :: filter_map ~f xs
end

module Option = struct
  let bind opt ~f =
    match opt with
    | None -> None
    | Some x -> f x

  let map ~f = function
    | None -> None
    | Some x -> Some (f x)

  let value ~default = function
    | None -> default
    | Some x -> x

  let value_map ~f ~default = function
    | None -> default
    | Some x -> f x

  module Infix = struct
    let return x  = Some x
    let (>>=) x f = bind x ~f
  end

  include Infix
end

let sprintf = Printf.sprintf
