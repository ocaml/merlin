open MenhirSdk
open Cmly_format

let name = ref ""
let verbose = ref false

let usage () =
  Printf.eprintf "Usage: %s [-v] file.cmly"
    Sys.argv.(0);
  exit 1

let () =
  for i = 1 to Array.length Sys.argv - 1 do
    if Sys.argv.(i) = "-v" then
      verbose := true
    else if !name = "" then
      name := Sys.argv.(i)
    else
      usage ()
  done;
  if !name = "" then
    usage ()

let g = Cmly_io.read_file !name

let reductions =
  let cmp_prod p1 p2 =
    compare p1.p_index p2.p_index
  in
  let table = Array.map (fun state ->
      state.lr1_reductions |>
      Array.to_list |>
      List.map snd |>
      List.concat |>
      List.sort_uniq cmp_prod
    ) g.g_lr1_states
  in
  fun state -> table.(state.lr1_index)

(** Misc routines *)

let is_attribute name (name', stretch : attribute) =
  name = Positions.value name'

let string_of_stretch s =
  s.Stretch.stretch_raw_content

let string_of_type = function
  | Stretch.Inferred s -> s
  | Stretch.Declared s -> string_of_stretch s

let name_of_symbol = function
  | T t -> t.t_name
  | N n -> n.n_name

let report fmt =
  if !verbose then
    Printf.eprintf fmt
  else
    Printf.ifprintf stderr fmt

let printf = Printf.printf
let sprintf = Printf.sprintf

let menhir = "MenhirInterpreter"

let align_tabular rows =
  let rec lengths l acc = function
    | [] when l = -1 -> []
    | [] ->
      l :: lengths (-1) [] acc
    | [] :: rows ->
      lengths l acc rows
    | (col :: cols) :: rows ->
      lengths (max l (String.length col)) (cols :: acc) rows
  in
  let lengths = lengths (-1) [] rows in
  let rec adjust_length lengths cols = match lengths, cols with
    | l :: ls, c :: cs ->
      let pad = l - String.length c in
      let c =
        if pad = 0 then c
        else c ^ String.make pad ' '
      in
      c :: adjust_length ls cs
    | _, [] -> []
    | [], _ -> assert false
  in
  List.map (adjust_length lengths) rows

let items_table items annots =
  let last_lhs = ref (-1) in
  let prepare (p,pos) annot =
    let rhs = Array.map (fun (sym, id, _) ->
        if id <> "" && id.[0] <> '_' then
          "(" ^ id ^ " = " ^ name_of_symbol sym ^ ")"
        else name_of_symbol sym)
        p.p_rhs
    in
    if pos >= 0 && pos < Array.length rhs then
      rhs.(pos) <- ". " ^ rhs.(pos)
    else if pos > 0 && pos = Array.length rhs then
      rhs.(pos - 1) <- rhs.(pos - 1) ^ " .";
    let rhs = Array.to_list rhs in
    let rhs =
      if !last_lhs = p.p_lhs.n_index then
        "" :: "  |" :: rhs
      else
        (last_lhs := p.p_lhs.n_index;
         p.p_lhs.n_name :: "::=" :: rhs)
    and annot =
      "" :: "" :: annot
    in
    [rhs; annot]
  in
  let annots = annots @ Array.to_list (Array.make (List.length items - List.length annots) []) in
  align_tabular (List.concat (List.map2 prepare items annots))

let report_table ?(prefix="") ?(sep=" ") table =
  List.iter (fun line -> report "%s%s\n" prefix (String.concat sep line))
    table

module Lr1s = CompressedBitSet.Make (struct
    type t = lr1_state
    let of_int i = g.g_lr1_states.(i)
    let to_int t = t.lr1_index
  end)

let lr1s_bind t f =
  Lr1s.fold (fun state states -> Lr1s.union (f state) states) t Lr1s.empty

exception Found of int
let array_exists arr f =
  try
    for i = 0 to Array.length arr - 1 do
      if f arr.(i) then raise (Found i);
    done;
    false
  with Found _ -> true

let array_findi arr f =
  match
    for i = 0 to Array.length arr - 1 do
      if f arr.(i) then raise (Found i);
    done
  with () -> raise Not_found
     | exception (Found i) -> i

let array_find arr f =
  arr.(array_findi arr f)

let array_assoc arr x =
  snd (array_find arr (fun (x',_) -> compare x x' = 0))

(*
let initial_state s = (fst s.lr1_lr0.lr0_items.(0)).p_rhs = [||]

let fst3 (x,_,_) = x
let snd3 (_,x,_) = x
let thd3 (_,_,x) = x

let follow_transition state sym =
  snd (state.lr1_transitions.(array_findi state.lr1_transitions
                                (fun x -> fst x = sym)))

let transition_order state =
  if initial_state state then [||]
  else
  let initial = function
    | T _, _ -> max_int
    | N n, state' ->
      if array_exists state'.lr1_lr0.lr0_items (fun (_,p) -> p > 1)
      then 0
      else max_int
  in
  let order = Array.map initial state.lr1_transitions in
  let changed = ref true in
  let update i = function
    | N n, state' when order.(i) = max_int ->
      Array.iter
      begin fun (prod,_) -> match
          array_findi state.lr1_transitions
            (fun (sym,_) -> sym = N prod.p_lhs)
        with
        | exception Not_found -> assert false
        | j ->
          if order.(j) < max_int then (
            order.(i) <- order.(j) + 1;
            changed := true
          )
      end state'.lr1_lr0.lr0_items
    | _ -> ()
  in
  while !changed do
    changed := false;
    Array.iteri update state.lr1_transitions
  done;
  order

let transition_order =
  let orders = Array.map transition_order g.g_lr1_states in
  fun state nt ->
    if initial_state state then 0
    else
      let order = orders.(state.lr1_index) in
      order.(array_findi state.lr1_transitions (fun (sym',_) -> N nt = sym'))

let () =
  let can_recover st item =
    match item.p_rhs.(0) with
    | T _, _, _ -> true
    | N n, _, _ ->
      let order = transition_order st n in
      if transition_order st item.p_lhs < order then true
      else begin
        let st' = follow_transition st (N item.p_lhs) in
        Array.fold_left (fun order (prod,p) ->
            if p > 1 then 0
            else
            min order (transition_order st prod.p_lhs))
          max_int st'.lr1_lr0.lr0_items
          < order
      end
  in
  let report_state st =
    let items = Array.to_list st.lr1_lr0.lr0_items in
    let p = List.fold_left max 0 (List.map snd items) in
    if p = 0 || (fst (List.hd items)).p_rhs = [||] then ()
    else
      let items = List.filter (fun (_,p') -> p = p') items in
      let items = List.map fst items in
      let preds = Pred.list p st in
      let count preds = string_of_int (Lr1s.cardinal preds) in
      report "# predecessors of %d(lr1)\n" st.lr1_index;
      report_table (items_table
                      (List.map (fun i -> i,p) items)
                      [List.map count preds]);
      begin match
        Lr1s.fold (fun st' lhs' ->
            let lhs' = List.filter (can_recover st') lhs' in
            if lhs' = [] && List.filter (can_recover st') items = [] then
              raise Exit;
            lhs'
          )
          (List.hd preds)
          items
      with
      | exception Exit -> report "Cannot recover\n"
      | [] -> report "Can recover, reductions depend on predecessor"
      | xs -> report "Can recover on %s\n"
                (String.concat ", " (List.map (fun p -> p.p_lhs.n_name) xs))
      end;
      report "\n"
  in
  Array.iter report_state g.g_lr1_states
  *)

module Synthesis = struct

  let symbol_cost (_ : symbol) = 1.0
  let prod_cost (_ : production) = 0.0

  type action =
    | Shift of symbol
    | Reduce of production
    | Sub of action list

  type item = (production * int)

  type 'a t = {
    value: 'a;
    actions: action list;
    cost: float;
  }

  module Stack = struct
    type t = (lr1_state * Lr1s.t) list

    let compare : t -> t -> int =
      let rec cmp l1 l2 = match l1, l2 with
        | [], [] -> (0)
        | _ , [] -> (1)
        | [], _  -> (-1)
        | ((st1, _) :: l1), ((st2, _) :: l2) ->
          begin match compare st1 st2 with
            | 0 -> cmp l1 l2
            | n -> n
          end
      in
      cmp
  end

  module StackMap = Map.Make(Stack)

  let rec merge_stacks s1 s2 =
    match s1, s2 with
    | [], [] -> []
    | ((st1, set1) :: s1'), ((st2, set2) :: s2') ->
      assert (st1 == st2);
      if Lr1s.equal set1 set2 then s1
      else (st1, Lr1s.union set1 set2) :: merge_stacks s1' s2'
    | _ -> assert false

  let add_stack thread map =
    match StackMap.find thread.value map with
    | thread' ->
      let stack = merge_stacks thread.value thread'.value in
      let thread = if thread.cost >= thread'.cost then thread' else thread in
      StackMap.add stack {thread with value = stack} map
    | exception Not_found ->
      let c = StackMap.cardinal map in
      if c > 100 then
        report "cardinal: %d\n thread: %s\n" c
          (String.concat ", " (List.map
                            (fun (st,_) -> string_of_int st.lr1_index) thread.value));
      StackMap.add thread.value thread map

  module Result = struct
    type t = item

    let compare (p1,pos1 : t) (p2,pos2 : t) =
      match compare p1.p_lhs.n_index p2.p_lhs.n_index with
      | 0 -> compare pos1 pos2
      | n -> n
  end
  module ResultMap = Map.Make(Result)

  let add_result result map =
    match ResultMap.find result.value map with
    | result' ->
      if result.cost >= result'.cost then
        map
      else ResultMap.add result.value result map
    | exception Not_found ->
      ResultMap.add result.value result map

  type step =
    | Result of item t
    | Thread of (lr1_state * Lr1s.t) list t
    | Abort

  let rec drop_n n = function
    | _ :: xs when n > 0 -> drop_n (n - 1) xs
    | xs -> n, xs

  let follow ?target thread action cost sym =
    let cost = thread.cost +. cost in
    if cost = infinity then
      None (* Unreachable *)
    else match thread.value with
    | [] -> assert false
    | (st, seen) :: stack ->
      let st' = match target with
        | Some st' -> st'
        | None ->
          let st' = array_assoc st.lr1_transitions sym in
          (*report "follow %d -> %d\n" st.lr1_index st'.lr1_index;*)
          st'
      in
      if Lr1s.mem st' seen then
        None (* Possibly entering cycle *)
      else
        let seen = Lr1s.add st' seen in
        let actions = action :: thread.actions in
        let value = (st', seen) :: (st, seen) :: stack in
        Some {actions; cost; value}

  let result thread prod pos =
    { actions = List.rev thread.actions;
      cost = thread.cost;
      value = (prod, pos + 1) }

  let shift thread (sym, st) =
    follow ~target:st thread (Shift sym) (symbol_cost sym) sym

  let reduce thread prod pos =
    (*report "Reducing:\n";
    report_table (items_table [(prod, pos)] []);
    report "From:\n";
    let items = Array.to_list (fst (List.hd thread.value)).lr1_lr0.lr0_items in
    report_table (items_table items []);*)
    let pos, stack = drop_n pos thread.value in
    match stack with
    | [] ->
      Result (result thread prod pos)
    | value ->
      match follow {thread with value}
              (Reduce prod) (prod_cost prod) (N prod.p_lhs) with
      | exception Not_found ->
        report "Failure, reducing:\n";
        report_table (items_table [(prod, pos)] []);
        report "From:\n";
        let items = Array.to_list (fst (List.hd value)).lr1_lr0.lr0_items in
        report_table (items_table items []);
        Abort
      | None -> Abort
      | Some t -> Thread t

  let apply_step (results, threads) = function
    | Abort -> (results, threads)
    | Result r -> (add_result r results, threads)
    | Thread t -> (results, add_stack t threads)

  let progress thread (results, threads) =
    let state, _ = List.hd thread.value in
    let threads = Array.fold_left
        (fun threads transition ->
           match shift thread transition with
           | None -> threads
           | Some thread -> add_stack thread threads)
        threads state.lr1_transitions
    in
    let results, threads = List.fold_left
        (fun acc prod ->
           apply_step acc (reduce thread prod (Array.length prod.p_rhs)))
        (results, threads) (reductions state)
    in
    (results, threads)

  let table : [`None | `Started | `Some of (item t) ResultMap.t] array =
    Array.make (Array.length g.g_lr1_states) `None

  let apply_results thread =
    let apply_result _ result acc =
      let thread = { actions = Sub result.actions :: thread.actions;
                     cost = result.cost +. thread.cost;
                     value = thread.value } in
      let prod, pos = result.value in
      (*report "reduce (%d, %d) on %s\n"
        prod.p_index (pos + 1)
        (String.concat "::" (List.map (fun (st,_) -> string_of_int st.lr1_index)
                               thread.value));*)
      apply_step acc (reduce thread prod pos)
    in
    ResultMap.fold apply_result

  let todo = ref []
  let progress solve _ thread acc =
    let st, stopset = List.hd thread.value in
    match table.(st.lr1_index) with
    | `None ->
      table.(st.lr1_index) <- `Started;
      todo := string_of_int st.lr1_index :: !todo;
      report "Solving state %d\n" st.lr1_index;
      let results = solve ~stopset st in
      todo := List.tl !todo;
      report "Solved state %d (todo: %s):\n" st.lr1_index (String.concat ", " !todo);
      let items = Array.to_list st.lr1_lr0.lr0_items in
      report_table (items_table items []);
      report "Reduce:\n";
      report_table (items_table
                      (List.map (fun (_,r) -> r.value)
                         (ResultMap.bindings results)) []);
      report "%!";
      table.(st.lr1_index) <- `Some results;
      apply_results thread results acc
    | `Some results ->
      apply_results thread results acc
    | `Started -> progress thread acc

  let rec solve ~stopset st =
    let rec loop (results, threads) =
      if StackMap.is_empty threads then results else
        loop (StackMap.fold (progress solve) threads (results, StackMap.empty))
    in
    let stack = [st, Lr1s.add st stopset] in
    loop (ResultMap.empty, StackMap.singleton stack { actions = []; cost = 0.0; value = stack})

  let solutions = Array.map (solve ~stopset:Lr1s.empty) g.g_lr1_states

  let () = Array.iteri (fun index results ->
      report "state: %d, solutions: [%s]\n"
        index (String.concat ", "
                 (List.map (fun (_,r) -> string_of_float r.cost)
                    (ResultMap.bindings results)))
    ) solutions

  let rec expand acc thread =
    let state, _ = List.hd thread.value in
    let acc = match reductions state with
      | [] -> acc
      | reds ->
        report "thread:";
        List.iter (fun (st,_)-> report " %d" st.lr1_index) thread.value;
        report "\n%!";
        thread :: acc
    in
    Array.fold_left
        (fun acc transition ->
           match shift thread transition with
           | None -> acc
           | Some thread ->
             expand acc thread)
        acc state.lr1_transitions

  let expand st =
    expand [] {cost = 0.0; value = [st, Lr1s.singleton st]; actions = []}

  let () = Array.iter (fun state ->
      report "state: %d, expansions: %d\n%!"
        state.lr1_index (List.length (expand state))
    ) g.g_lr1_states

end

(** Predecessors *)

module Pred = struct
  let imm =
    let tbl1 = Array.make (Array.length g.g_lr1_states) Lr1s.empty in
    let revert_transition s1 (sym,s2) =
      assert (match s2.lr1_lr0.lr0_incoming with
          | None -> false
          | Some sym' -> sym = sym');
      tbl1.(s2.lr1_index) <- Lr1s.add s1 tbl1.(s2.lr1_index)
    in
    Array.iter
      (fun lr1 -> Array.iter (revert_transition lr1) lr1.lr1_transitions)
      g.g_lr1_states;
    (fun lr1 -> tbl1.(lr1.lr1_index))

  let nth nth n st = match n with
    | 0 -> Lr1s.singleton st
    | n ->
      assert (n > 0);
      lr1s_bind (nth (n - 1) st) imm

  let nth =
    let table = Hashtbl.create 119 in
    let rec fix n st =
      let key = (n, st.lr1_index) in
      try Hashtbl.find table key
      with Not_found ->
        let result = nth fix n st in
        Hashtbl.add table key result;
        result
    in
    fix

  let list n st =
    let preds = ref [] in
    for i = 0 to n do
      preds := nth i st :: !preds
    done;
    !preds
end
