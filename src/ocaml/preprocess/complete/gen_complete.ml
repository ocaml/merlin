open MenhirSdk

include Cmly_read.Read(struct let filename = Sys.argv.(1) end)

let is_attribute names attr =
  List.exists (fun l -> Attribute.has_label l attr) names

let printf = Printf.printf
let sprintf = Printf.sprintf

let menhir = "MenhirInterpreter"

let visited = Hashtbl.create 7

let rec intersect lx ly =
  match lx, ly with
  | [], _ | _, [] -> []
  | x :: xs, y :: ys ->
    if x < y then
      intersect xs ly
    else if x > y then
      intersect lx ys
    else
      x :: intersect xs ys

let rec union lx ly =
  match lx, ly with
  | [], r | r, [] -> r
  | x :: xs, y :: ys ->
    if x < y then
      x :: union xs ly
    else if x > y then
      y :: union lx ys
    else
      x :: union xs ys

let rec merge la = function
  | [], x -> x
  | x, [] -> List.map (fun r -> r, la) x
  | (r1 :: rs1 as rrs1), ((p2, la2 as r2) :: rs2 as rrs2) ->
    if r1 = p2 then
      (r1, union la la2) :: merge la (rs1, rs2)
    else if r1 < p2 then
      (r1, la) :: merge la (rs1, rrs2)
    else
      r2 :: merge la (rrs1, rs2)

let rec augment_reductions lookahead l1 l2 =
  match l1, l2 with
  | [], r -> r
  | r, [] -> List.map (List.map (fun r -> r, lookahead)) r
  | x1 :: r1, x2 :: r2 ->
    merge lookahead (x1, x2) :: augment_reductions lookahead r1 r2

let lookahead_by_reductions =
  Lr1.tabulate begin fun lr1 ->
    let all_reduce =
      List.map (fun (token, prods) -> (List.hd prods, token)) (Lr1.reductions lr1)
    in
    let by_prod =
      let pack prod tokens rest = (prod, List.sort compare tokens) :: rest in
      let rec regroup prod tokens = function
        | [] -> pack prod tokens []
        | (prod', token') :: rest ->
          if prod = prod'
          then regroup prod (token' :: tokens) rest
          else pack prod tokens (regroup prod' [token'] rest)
      in
      match List.sort compare all_reduce with
      | [] -> []
      | (prod, token) :: rest -> regroup prod [token] rest
    in
    by_prod
  end

let compact_lookahead la =
  let max i t = let t = Terminal.to_int t in if t < i then i else t in
  let maximum = List.fold_left max (-1) la in
  let buf = Bytes.make ((maximum + 8) / 8) '\000' in
  List.iter (fun idx ->
      let idx = Terminal.to_int idx in
      let offset = idx / 8 in
      let mask = 1 lsl (idx mod 8) in
      Bytes.set buf offset (Char.chr (mask lor Char.code (Bytes.get buf offset)))
    ) la;
  Bytes.to_string buf

let compact_lookahead =
  let table = Hashtbl.create 7 in
  fun la -> try Hashtbl.find table la with Not_found ->
    let result = compact_lookahead la in
    Hashtbl.add table la result;
    result

let closure lr1 =
  let export_reduction prod i =
    let rec aux = function
      | 0 -> [[Production.lhs prod]]
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
  let rec visit lookahead = function
    | [] -> assert false
    | stack when Hashtbl.mem visited (stack, lookahead) -> ()
    | (top :: _) as stack ->
      Hashtbl.add visited (stack, lookahead) top;
      let productions = lookahead_by_reductions top in
      List.iter (fun (prod, lookahead') ->
          let lookahead = match stack with
            | [_] -> lookahead'
            | _ -> intersect lookahead lookahead'
          in
          if lookahead <> [] then
            match simulate prod stack (Array.length (Production.rhs prod)) with
            | Ok stack' -> visit lookahead stack'
            | Error reduction -> reductions :=
                augment_reductions lookahead reduction !reductions
        ) productions
  in
  visit [] [lr1];
  let reductions =
    List.map (List.map (fun (p, la) -> p, compact_lookahead la)) !reductions
  in
  let visited =
    Hashtbl.fold (fun _ top acc -> if top <> lr1 then top :: acc else acc) visited []
    |> List.sort_uniq compare
  in
  (reductions, visited)

type itemsets = (int * int) array array

let itemsets0 =
  Lr0.tabulate begin fun lr0 ->
    let to_item (prod, dot) = (Production.to_int prod, dot) in
    Lr0.items lr0
    |> List.map to_item
    |> Array.of_list
  end


let () =
  let red_tbl = Array.init Lr1.count (fun lr1 -> closure (Lr1.of_int lr1)) in
  let goto_tbl = Array.init Lr1.count (fun lr1 ->
      List.filter_map (function
          | (N nt, target) -> Some (Nonterminal.to_int nt, Lr1.to_int target)
          | (T _, _) -> None
        ) (Lr1.transitions (Lr1.of_int lr1))
    )
  in
  let items_tbl : itemsets =
    Array.init Lr1.count (fun lr1 -> itemsets0 (Lr1.lr0 (Lr1.of_int lr1)))
  in
  Printf.printf "let reduction_table : \
                 ((int * string) list list * int list) array lazy_t =\n\
                \  lazy (Marshal.from_string %S 0)\n"
    (Marshal.to_string red_tbl []);
  Printf.printf "let goto_table : (int * int) list array lazy_t =\n\
                \  lazy (Marshal.from_string %S 0)\n"
    (Marshal.to_string goto_tbl []);
  Printf.printf "let items_table : (int * int) array array lazy_t =\n\
                \  lazy (Marshal.from_string %S 0)\n"
    (Marshal.to_string items_tbl []);
  print_endline "open Parser_raw";
  print_endline "let productions = MenhirInterpreter.[|";
  Production.iter begin fun prod ->
    print_string "  [|";
    if Production.kind prod = `REGULAR then
      Array.iter (fun (symbol, _, _) ->
          match symbol with
          | T t -> Printf.printf "X (T T_%s);" (Terminal.name t)
          | N n -> Printf.printf "X (N N_%s);" (Nonterminal.mangled_name n)
        ) (Production.rhs prod);
    print_endline "|];";
  end;
  print_endline "|]";
  let first_real_nt =
    let exception Found of int in
    try
      Nonterminal.iter (fun nt -> if Nonterminal.kind nt = `REGULAR then
                           raise (Found (Nonterminal.to_int nt)));
      assert false
    with Found nt -> nt
  in
  let nonterminals_tbl = Array.make (Nonterminal.count - first_real_nt) [] in
  Production.iter (fun p ->
      if Production.kind p = `REGULAR then (
        let lhs = Nonterminal.to_int (Production.lhs p) - first_real_nt in
        nonterminals_tbl.(lhs) <- Production.to_int p :: nonterminals_tbl.(lhs)
      )
    );
  Printf.printf "let nonterminal_prods : int list array lazy_t =\n\
                \  lazy (Marshal.from_string %S 0)\n"
    (Marshal.to_string nonterminals_tbl [])
