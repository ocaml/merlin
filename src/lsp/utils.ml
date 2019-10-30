module Result = struct

  let bind x ~f =
    match x with
    | Ok v -> f v
    | Error err -> Error err

  let map x ~f =
    match x with
    | Ok v -> Ok (f v)
    | Error err -> Error err

  let errorf fmt =
    let kerr _ =  Error (Format.flush_str_formatter ()) in
    Format.kfprintf kerr Format.str_formatter fmt

  module Infix = struct
    let (>>=) x f = bind x ~f
    let (>>|) x f = map x ~f
    let return x = Ok x
    let errorf = errorf
  end
end
