let fun_name1 () = ()

let all_empty l =
  List.for_all
    (function
      | [] -> true
      | _ -> false)
    l

let max l = List.fold_left (fun acc x -> if x > acc then x else acc) l

(* A comment *)
let z = "..."

let test x y =
  let fun_name2 = Fun.id in
  let m =
    let m = print_endline (x ^ y ^ z) in
    m
  in
  m

let rec x = object end

and y _ =
  object
    method foo = x
  end

let rec z x = 10 + y + x

and y = 80

let f =
  print_endline "Wild side effect!";
  1 :: [ 2; 3; 4 ]

class a =
  let inner_expr =
    let bar = 20 in
    object
      method foo = bar
    end
  in
  object
    method x = (Fun.const 10) ()
    method y = print_endline
    method z =
      let x =
        object
          method x = "foobar"
        end
      in
      x
  end

and b = object end

let my_mutable_state =
  let var = ref 0 in
  var := y * 50;
  !var

let func () =
  let x = [] in
  Fun.protect
    (fun () ->
      let fun_name2 = ( / ) in
      let y = [ ( + ); ( - ); fun_name2 ] @ x in
      List.map2 (fun op (a, b) -> op a b) y [ (1, 1); (3, 2); (8, 2) ])
    ~finally:(Fun.const ())

let rec f = List.map Fun.id

and y = [ 10; 20; 30 ]

and z x =
  object
    method x = x
    method y = y
  end

let f = 0 + 1

let f x = (x * 2) + 3

let f x =
  let y = 0 in
  (x * y) + 3

let f x =
  let exception Local in
  raise Local

let x = 0
let f x = x + 1

let x = 0
let y = 1
let f x = x + y

let f x = List.map (fun y -> y + 1) x

let f y =
  let y = y + 1 in
  y + 2

let f () = y + 1

let f x =
  let module M = struct
    let y = 0
  end in
  (x * M.y) + 3

let f =
  let x = 1 in
  let y = 2 in
  let z = x + y in
  z + z + 1

type document = markup list
and markup = Text of string | Bold of string

let pp_document ppf doc =
  let open Format in
  let bold_tag = "**" in
  fprintf ppf "%a"
    (pp_print_list (fun ppf markup ->
         match markup with
         | Text txt -> pp_print_string ppf txt
         | Bold txt -> pp_print_string ppf (bold_tag ^ txt ^ bold_tag)))
    doc

module A = struct
  let a = 10
end
let f x =
  let module Empty = struct end in
  let module M = struct
    module MM = struct
      let y = 0
    end
    let z = 0
  end in
  (x * M.z * M.MM.y) + A.a

module T = struct
  let on_list x = x + 1
  let k : (int, int) Result.t = Ok 10
  let r = Ok 10

  let x =
    let a_list = List.map on_list [ 1; 2; 3 ] in
    let open Format in
    let printer = pp_print_list pp_print_int in
    printf "%a\n" printer a_list
end

let z = 100

let complicated_function x y =
  let a = 10 in
  let b = 11 in
  let c = 12 in
  let module D = struct
    let x = 13
  end in
  a + b + (c * x * y) + z + D.x
