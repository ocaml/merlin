open MenhirSdk
open Cmly_format

let name = ref ""
let verbose = ref false

let usage () =
  Printf.eprintf "Usage: %s [-v] file.cmly\n"
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

(** Misc routines *)

let list_fmt f l =
  "["^String.concat "; " (List.map f l)^"]"

let fst3 (x,_,_) = x

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
  List.concat (List.map2 prepare items annots)

let report_table ?(prefix="") ?(sep=" ") table =
  let table = align_tabular table in
  List.iter (fun line -> report "%s%s\n" prefix (String.concat sep line))
    table

(* Dump all productions and states for debugging *)

let () =
  Array.iter (fun p ->
      report
        "# p%d (%s)\n"
        p.p_index
        (if p.p_kind = `START then "start" else "regular");
      report_table (items_table [(p,-1)] [])
    ) g.g_productions

let () =
  Array.iter (fun st ->
      report "# #%d\n" st.lr1_index;

      report "transitions:\n";
      Array.iter
        (fun (sym, st') -> report " %s -> %d\n" (name_of_symbol sym) st'.lr1_index)
        st.lr1_transitions;

      report "reductions:\n";
      Array.iter
        (fun (t, prods) -> report " %s -> %s\n" t.t_name
            (list_fmt (fun p -> "p"^string_of_int p.p_index) prods))
        st.lr1_reductions;

      report "items:\n";
      report_table (items_table (Array.to_list st.lr1_lr0.lr0_items) []);
    ) g.g_lr1_states

module Lr1s = MakeBitSet.Make (struct
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

module Pred = struct
  let imm =
    let tbl1 = Array.make (Array.length g.g_lr1_states) [] in
    let revert_transition s1 (sym,s2) =
      assert (match s2.lr1_lr0.lr0_incoming with
          | None -> false
          | Some sym' -> sym = sym');
      tbl1.(s2.lr1_index) <- s1 :: tbl1.(s2.lr1_index)
    in
    Array.iter
      (fun lr1 -> Array.iter (revert_transition lr1) lr1.lr1_transitions)
      g.g_lr1_states;
    (fun lr1 -> tbl1.(lr1.lr1_index))

  let step acc = function
    | [] -> assert false
    | (st :: _) as stack ->
      List.fold_left (fun acc st' -> (st' :: stack) :: acc) acc (imm st)

  let stacks n st =
    let stacks = ref [[st]] in
    for i = 0 to n do
      stacks := List.fold_left step [] !stacks
    done;
    !stacks

end

(* negation to put nan as the max *)
let compare_float a b = - compare (-.a) (-.b)

let min_float a b =
  if compare_float a b > 0 then b else a

module Synthesis = struct

  type variable =
    | Head of lr1_state * nonterminal
    | Tail of lr1_state * production * int

  module Solver = Fix.Make (struct
      type key = variable
      type 'a t = (key, 'a) Hashtbl.t
      let create () = Hashtbl.create 7
      let find k tbl = Hashtbl.find tbl k
      let add k v tbl = Hashtbl.add tbl k v
      let iter f tbl = Hashtbl.iter f tbl
      let clear = Hashtbl.clear
    end) (struct
      type property = float
      let bottom = infinity
      let equal : float -> float -> bool = (=)
      let is_maximal f = f = 0.0
    end)

  let cost_of_prod (prod : production) = 0.0

  let penalty_of_item (_ : production * int) = 0.0

  let cost_of_symbol = function
    | T _ -> 1.0
    | N _ -> infinity

  let const c = fun _ -> c
  let app var v = v var

  let var var = match var with
    | Head _ -> app var
    | Tail (_,prod,pos) ->
      if pos < Array.length prod.p_rhs then
        app var
      else
        let cost = cost_of_prod prod in
        const cost

  let eval = function
    | Head (st, n) ->
      let acc = Array.fold_left
          (fun acc (sym, st') ->
             Array.fold_left (fun acc (prod, pos) ->
                 if pos = 1 && prod.p_lhs = n then
                   var (Tail (st, prod, 0)) :: acc
                 else acc
               ) acc st'.lr1_lr0.lr0_items
          ) [] st.lr1_transitions
      in
      let cost = Array.fold_left
          (fun acc (_, prods) ->
             List.fold_left (fun acc prod ->
                 if prod.p_rhs = [||] && prod.p_lhs = n then
                   min_float (cost_of_prod prod) acc
                 else acc
               ) acc prods
          ) infinity st.lr1_reductions
      in
      if cost < infinity || acc <> [] then
        (fun v -> List.fold_left (fun cost f -> min_float cost (f v)) cost acc)
      else const infinity

    | Tail (st, prod, pos) ->
      let penalty = penalty_of_item (prod, pos) in
      if penalty = infinity then
        const infinity
      else
      if pos >= Array.length prod.p_rhs then
        const (cost_of_prod prod)
      else
        let head =
          let sym, _, _ = prod.p_rhs.(pos) in
          let cost = cost_of_symbol sym in
          if cost < infinity then const cost
          else match sym with
            | T _ -> const infinity
            | N n -> var (Head (st, n))
        in
        let tail =
          match array_assoc st.lr1_transitions (fst3 prod.p_rhs.(pos)) with
          | st' -> var (Tail (st', prod, pos + 1))
          | exception Not_found ->
            report "no transition: #%d (%d,%d)\n" st.lr1_index prod.p_index pos;
            const infinity
        in
        (fun v -> head v +. tail v)

  let group_assoc l =
    let cons k v acc = (k, List.rev v) :: acc in
    let rec aux k v acc = function
      | [] -> List.rev (cons k v acc)
      | (k', v') :: xs when compare k k' = 0 ->
        aux k (v' :: v) acc xs
      | (k', v') :: xs ->
        aux k' [v'] (cons k v acc) xs
    in
    match List.sort compare l with
    | [] -> []
    | (k, v) :: xs -> aux k [v] [] xs

  let cost_of = Solver.lfp eval

  let () =
    let solutions =
      Array.fold_left (fun acc st ->
          match Array.fold_left (fun (item, cost) (prod, pos) ->
              let cost' = cost_of (Tail (st, prod, pos)) in
              if cost' < cost then (Some (prod, pos), cost') else (item, cost)
            ) (None, infinity) st.lr1_lr0.lr0_items
          with
          | None, _ -> report "no synthesis from %d\n" st.lr1_index; acc
          | Some item, cost -> (item, (cost, st.lr1_index)) :: acc
        ) [] g.g_lr1_states
    in
    List.iter (fun (item, states) ->
        report "# Item (%d,%d)\n" (fst item).p_index (snd item);
        report_table (items_table [item] []);
        List.iter (fun (cost, states) ->
            report "at cost %f from states %s\n\n"
              cost (String.concat ", " (List.map string_of_int states))
          ) (group_assoc states);
      ) (group_assoc solutions)

end

module Recovery = struct

  type item = lr1_state * production * int

  module ItemsH = Hashtbl.Make(struct
      type t = item list
      let hash = function
        | (lr1, prod, pos) :: _ ->
            Hashtbl.hash (lr1.lr1_index, prod.p_index, pos)
        | [] -> 0
      let equal a b = compare a b = 0
    end)

  type trace = Trace of float * item list

  let min_trace (Trace (c1, _) as tr1) (Trace (c2, _) as tr2) =
    if c2 < c1 then tr2 else tr1

  let cat_trace (Trace (c1, tr1)) (Trace (c2, tr2)) =
    Trace (c1 +. c2, tr1 @ tr2)

  let rec merge_nts l1 l2 = match l1, l2 with
    | [], l -> l
    | l, [] -> l
    | ((nt1, c1) :: xs1), (x2 :: xs2) ->
      let (nt2, c2) = x2 in
      match compare nt1.n_index nt2.n_index with
      | 0 ->
        let x = (nt1, min_trace c1 c2) in
        x :: merge_nts xs1 xs2
      | n when n > 0 -> x2 :: merge_nts l1 xs2
      | _ -> (nt1, c1) :: merge_nts xs1 l2

  let rec merge l1 l2 = match l1, l2 with
    | [], l -> l
    | l, [] -> l
    | (x1 :: l1), (x2 :: l2) ->
      let x' = merge_nts x1 x2 in
      x' :: merge l1 l2

  let synthesize =
    let rec add_nt cost nt = function
      | [] -> [(nt, cost)]
      | x :: xs ->
          let c = compare nt.n_index (fst x).n_index in
          if c = 0 then (nt, min_trace cost (snd x)) :: xs
          else if c < 0 then
            (nt, cost) :: xs
          else
            x :: add_nt cost nt xs
    in
    let add_item cost item stack =
      let (_, prod, pos) = item in
      if cost = infinity then stack
      else
        let stack_hd = function
          | [] -> []
          | x :: _ -> x
        and stack_tl = function
          | [] -> []
          | _ :: xs -> xs
        in
        let rec aux stack = function
          | 0 -> add_nt (Trace (cost, [item])) prod.p_lhs (stack_hd stack) :: stack_tl stack
          | n -> stack_hd stack :: aux (stack_tl stack) (n - 1)
        in
        aux stack pos
    in
    let table = Array.map (fun st ->
        Array.fold_left (fun acc (prod, pos) ->
            if pos = 0 then (
            (*if prod.p_kind = `START then ( *)
              (* pos = 0 means we are on an initial state *)
              (*report "skipping %s at depth %d\n" prod.p_lhs.n_name pos;*)
              acc
            ) else (
              (*report "adding %s at depth %d\n" prod.p_lhs.n_name pos;*)
              add_item
                (Synthesis.cost_of (Synthesis.Tail (st, prod, pos)))
                (st, prod, pos) acc
            )
          )
          [] st.lr1_lr0.lr0_items
      ) g.g_lr1_states
    in
    fun st -> table.(st.lr1_index)

  let print_interp_state ((st, sts), traces) =
    let print_trace (nt, Trace (cost, items)) =
      sprintf "%S, Trace (%f, [%s])"
        nt.n_name cost (list_fmt
                          (fun (st,prod,pos) -> sprintf "(#%d, p%d, %d)" st.lr1_index prod.p_index pos)
                          items)
    in
    sprintf "(#%d, [%s], [%s])"
      st.lr1_index (list_fmt string_of_int sts) (list_fmt (list_fmt print_trace) traces)

  let step st ntss =
    let seen = ref CompressedBitSet.empty in
    let rec aux = function
      | [] -> []
      | ((nt, cost) :: x) :: xs
        when not (CompressedBitSet.mem nt.n_index !seen) && not (nt.n_kind = `START) ->
          seen := CompressedBitSet.add nt.n_index !seen;
          let st' = array_assoc st.lr1_transitions (N nt) in
          (*Printf.eprintf "synthesize #%d " st'.lr1_index;*)
          let xs' = synthesize st' in
          (*Printf.eprintf "= %s\n" (print_interp_state ((st', [st'.lr1_index]), xs'));*)
          let xs' = match xs' with
            | [] -> []
            | _ :: xs -> xs
          in
          let xs' = List.map (List.map (fun (nt,c) -> (nt, cat_trace c cost))) xs' in
          aux (merge xs' (x :: xs))
      | (_ :: x) :: xs -> aux (x :: xs)
      | [] :: xs -> xs
    in
    aux ntss

  let total = ref 0

  let init st =
    let r = ((st, [st.lr1_index]), step st (synthesize st)) in
    (*Printf.eprintf "init #%d = %s\n" st.lr1_index (print_interp_state r);*)
    r

  let expand ((st, sts), nts) =
    List.map (fun st' -> ((st', st'.lr1_index :: sts), step st' nts)) (Pred.imm st)

  let recover st =
    let pos = Array.fold_left (fun pos (_, pos') -> max pos pos')
        (snd st.lr1_lr0.lr0_items.(0)) st.lr1_lr0.lr0_items
    in
    (* Walk the known part of the stack *)
    let traces =
      let acc = ref [init st] in
      for i = 1 to pos - 1 do
        acc := List.concat (List.map expand !acc)
      done;
      !acc
    in
    (* Code emission, with sharing of solutions *)
    let shared_defs = ref [] in
    let emit_traces =
      let sym_counter = ref 0 in
      let sym () = incr sym_counter; "sym" ^ string_of_int !sym_counter in
      let emit emit = function
        | [] -> assert false
        | (lr1, prod, pos) :: xs ->
            let xs = if xs = [] then "[]" else emit xs in
            let sym = sym () in
            shared_defs := (sym, sprintf "(%d, %d, %d) :: %s"
                              lr1.lr1_index prod.p_index pos xs) ::
                           !shared_defs;
            sym
      in
      let table = ItemsH.create 113 in
      let rec memo tr =
        match ItemsH.find table tr with
        | v -> v
        | exception Not_found ->
            let v = emit memo tr in
            ItemsH.add table tr v;
            v
      in
      memo
    in
    (* Last step *)
    let select_trace traces =
      (* Pick a trace with minimal cost, somehow arbitrary *)
      match List.flatten traces with
      | [] -> assert false
      | (_, trace) :: alternatives ->
          List.fold_left
            (fun (Trace (c1, _) as tr1) (_, (Trace (c2, _) as tr2)) ->
               if compare_float c1 c2 <= 0 then tr1 else tr2)
            trace alternatives
    in
    let process_trace trace =
      match expand trace with
      | [] -> (* Initial state *)
          assert (snd trace = []);
          []
      | states ->
          let select_expansion ((st, sts), trace') =
            if trace' = [] then
              (* Emptied the stack *)
              (None, select_trace (snd trace))
            else
              (Some st, select_trace trace')
          in
          List.map select_expansion states
    in
    let results =
      List.flatten @@
      List.map (fun trace ->
          List.map
            (fun (st, Trace (_, reductions)) ->
               (match st with None -> "-1"
                            | Some st -> string_of_int st.lr1_index) ,
               emit_traces reductions)
            (process_trace trace)
        ) traces
    in
    total := !total + (List.length !shared_defs);
    printf "let recover_%d =\n" st.lr1_index;
    List.iter (fun (sym, v) -> printf "  let %s = %s in\n" sym v) (List.rev !shared_defs);
    printf "  %d, function\n" pos;
    List.iter (fun (st, v) -> printf "  | %s -> %s\n" st v) results;
    printf "  | _ -> raise Not_found\n";
    []
    (*[ List.fold_left min infinity
        (List.map (fun (_, Trace (cost, _)) -> cost)
           (List.flatten (List.flatten solutions))) ]*)

  let () =
    report "# Recovery cost\n\n";
    Array.iter (fun st ->
        report "Recover #%d\n%!" st.lr1_index;
        let costs = List.sort_uniq compare (recover st) in
        report "%s\n%!"
          (String.concat ", " (List.map string_of_float costs))
      ) g.g_lr1_states;
    report "Explored %d\n" !total
end
