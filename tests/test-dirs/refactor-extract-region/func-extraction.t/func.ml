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

let my_object =
  object
    method foo =
      let () = () in
      begin
        let var = ref 0 in
        var := 10 * 50;
        !var
      end
  end

module type T = sig
  val add : int -> int -> int
end
let test (module X : T) (x) y (module Y : T) =
  let () = () in
  X.add x y
