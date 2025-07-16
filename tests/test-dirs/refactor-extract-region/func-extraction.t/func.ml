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

let map f =
  let rec loop acc = function
    | [] -> List.rev acc
    | x :: xs -> loop (f x :: acc) xs
  in
  loop []

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
  var := 10 * 50;
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
