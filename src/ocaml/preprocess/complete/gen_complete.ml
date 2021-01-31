open MenhirSdk

include Cmly_read.Read(struct let filename = Sys.argv.(1) end)

let is_attribute names attr =
  List.exists (fun l -> Attribute.has_label l attr) names

let printf = Printf.printf
let sprintf = Printf.sprintf

let menhir = "MenhirInterpreter"

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
