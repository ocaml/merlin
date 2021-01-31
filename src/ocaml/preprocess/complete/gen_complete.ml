open MenhirSdk

include Cmly_read.Read(struct let filename = Sys.argv.(1) end)

let is_attribute names attr =
  List.exists (fun l -> Attribute.has_label l attr) names

let printf = Printf.printf
let sprintf = Printf.sprintf

let menhir = "MenhirInterpreter"

let visited = Hashtbl.create 7

let rec augment_reductions r1 r2 =
  let rec merge = function
    | [], x | x, [] -> x
    | (l1 :: ls as lls), (r1 :: rs as rrs) ->
      if l1 = r1 then
        l1 :: merge (ls, rs)
      else if l1 < r1 then
        l1 :: merge (ls, rrs)
      else
        r1 :: merge (lls, rs)
  in
  match r1, r2 with
  | [], r | r, [] -> r
  | x1 :: r1, x2 :: r2 ->
    merge (x1, x2) :: augment_reductions r1 r2

let closure lr1 =
  let export_reduction prod i =
    let rec aux = function
      | 0 -> [[prod]]
      | i -> [] :: aux (i - 1)
    in
    aux i
  in
  let rec simulate prod stack i =
    match stack with
    | [] -> Error (export_reduction prod i)
    | hd :: tl ->
      if i > 0 then
        simulate prod tl (i - 1)
      else
        let lr1' = List.assoc (N (Production.lhs prod)) (Lr1.transitions hd) in
        Ok (lr1' :: stack)
  in
  Hashtbl.clear visited;
  let reductions = ref [] in
  let rec visit = function
    | [] -> assert false
    | stack when Hashtbl.mem visited stack -> ()
    | (top :: _) as stack ->
      Hashtbl.add visited stack top;
      let productions =
        List.map (fun (_, prods) -> List.hd prods) (Lr1.reductions top)
        |> List.sort_uniq compare
      in
      List.iter (fun prod ->
          match simulate prod stack (Array.length (Production.rhs prod)) with
          | Ok stack' -> visit stack'
          | Error reduction -> reductions := augment_reductions reduction !reductions
        ) productions
  in
  visit [lr1];
  let reductions = !reductions in
  let visited =
    Hashtbl.fold (fun _ top acc -> top :: acc) visited []
    |> List.sort_uniq compare
  in
  (reductions, visited)

let () =
  let stringlist f l =
    "[" ^ String.concat ";" (List.map f l) ^ "]"
  in
  Lr1.iter (fun lr1 ->
      let reductions, visited = closure lr1 in
      Printf.printf "%d: %s, %s\n"
        (Lr1.to_int lr1)
        (stringlist (stringlist (fun prod -> string_of_int (Production.to_int prod))) reductions)
        (stringlist (fun lr1 -> string_of_int (Lr1.to_int lr1)) visited)
    )

(*type arities = int array

let arities =
  Array.init Production.count begin fun prod ->
   Array.length (Production.rhs (Production.of_int prod))
  end

type reductions = int array array

let reductions : reductions =
  Array.init Lr1.count begin fun lr1 ->
    let to_prod (_, prods) = Production.to_int (List.hd prods) in
    Lr1.reductions (Lr1.of_int lr1)
    |> List.map to_prod
    |> List.sort_uniq compare
    |> List.sort (fun p1 p2 -> Int.compare arities.(p1) arities.(p2))
    |> Array.of_list
  end*)

(*type itemsets = (int * int) array array

let itemsets0 =
  Lr0.tabulate begin fun lr0 ->
    let to_item (prod, dot) = (Production.to_int prod, dot) in
    Lr0.items lr0
    |> List.map to_item
    |> Array.of_list
  end

let itemsets : itemsets =
  Array.init Lr1.count begin fun lr1 ->
    itemsets0 (Lr1.lr0 (Lr1.of_int lr1))
  end

let () =
  Printf.printf "let raw_arities : int array lazy_t = \
                   lazy (Marshal.from_string %S 0)\n\
                 let raw_reductions : int array array lazy_t = \
                   lazy (Marshal.from_string %S 0)\n\
                 let raw_itemsets : (int * int) array array lazy_t = \
                   lazy (Marshal.from_string %S 0)\n"
    (Marshal.to_string arities [])
    (Marshal.to_string reductions [])
    (Marshal.to_string itemsets [])
*)
