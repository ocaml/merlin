let const_name1 = "before"

let circle_area radius = 3.14159 *. (radius ** 2.)

let read ?(chunk_size = 4096) ic =
  let buf = Bytes.create chunk_size in
  In_channel.input ic buf 0 (Bytes.length buf)

(* My commentary *)
let my_nested_long_int =
  let o =
    let c =
      let a =
        let m =
          let l = 1_000_000_000L in
          l
        in
        m
      in
      a
    in
    c
  in
  o

let log ppf msg = Format.pp_print_string ppf ("CRITICAL: " ^ msg)

module type EMPTY = sig end
let f () : (module EMPTY) =
  (module struct
    let const_name2 = assert false
    let secret = String.make 100 '@'
  end)

let g () =
  let multilines_cst = {foo|
multi
lines
constant
|foo} in
  print_endline multilines_cst
