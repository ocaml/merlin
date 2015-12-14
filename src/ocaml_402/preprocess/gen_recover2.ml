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
  align_tabular (List.concat (List.map2 prepare items annots))

let report_table ?(prefix="") ?(sep=" ") table =
  List.iter (fun line -> report "%s%s\n" prefix (String.concat sep line))
    table

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

module Synthesis = struct
  type variable =
    | Shift of lr1_state * production * int
    | Synthesize of lr1_state * nonterminal

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

  let produce_nt st nt =
    Array.fold_left (fun acc (sym,st') ->
        match sym with
        | N _ -> acc
        | T t ->
          let cost = cost_of_symbol sym in
          if cost = infinity then acc else
            Array.fold_left (fun acc (prod,pos) ->
                if pos <> 1 || prod.p_lhs <> nt then acc else
                  (fun v -> cost +. v (st',prod,pos)) :: acc)
              acc st'.lr1_lr0.lr0_items)
      [] st.lr1_transitions

  let eval = function
    | Shift (st, prod, n) when n = Array.length prod.p_rhs ->
      (fun _ -> cost_of_prod prod)
    | var -> (fun v -> v var)

  let produce st (prod, pos) =
    let penalty = penalty_of_item (prod, pos) in
    if penalty = infinity then (fun _ -> infinity)
    else begin
      let sym, _, _ = prod.p_rhs.(pos) in
      let cost = penalty +. cost_of_symbol sym in
      match sym with
      | T _ -> (fun _ -> cost)
      | N _ when cost < infinity -> (fun _ -> cost)
      | N n ->
        let f = eval (Synthesize (st, n)) in
        (fun v -> penalty +. f v)
    end

  let eval = function
    | Synthesize (st, nt) ->
      let acc = [] in
      let acc = Array.fold_left
          (fun acc (sym, st') ->
             Array.fold_left (fun acc (prod, pos) ->
                 if pos = 1 && prod.p_lhs = nt then
                   let f0 = produce st (prod, 0) in
                   let f1 = eval (Shift (st', prod, 1)) in
                   (fun v -> f0 v +. f1 v) :: acc
                 else acc
               ) acc st'.lr1_lr0.lr0_items
          ) acc st.lr1_transitions
      in
      let acc = Array.fold_left
          (fun acc (_, prods) ->
             List.fold_left (fun acc prod ->
                 if prod.p_rhs = [||] && prod.p_lhs = nt then
                   (fun _ -> cost_of_prod prod) :: acc
                 else acc
               ) acc prods
          ) acc st.lr1_reductions
      in
      (fun v -> List.fold_left (fun cost f -> min cost (f v)) infinity acc)

    | Shift (st, prod, pos) ->
      let produce = produce st (prod, pos) in
      let tail =
        match array_assoc st.lr1_transitions (fst3 prod.p_rhs.(pos)) with
        | st' -> eval (Shift (st', prod, pos + 1))
        | exception Not_found -> (fun _ -> infinity)
      in
      (fun v -> produce v +. tail v)

  let eval = function
    | Shift (st, prod, n) when n >= Array.length prod.p_rhs ->
      (fun _ -> cost_of_prod prod)
    | var -> eval var

  let () =
    let solution = Solver.lfp eval in
    Array.iter (fun st ->
        report "# State %d\n%!" st.lr1_index;
        Array.iter (fun (prod,pos) ->
            report_table (items_table [(prod,pos)] []);
            report "cost:%!";
            report " %f\n\n%!" (solution (Shift (st,prod,pos)))
          ) st.lr1_lr0.lr0_items;
      ) g.g_lr1_states
end
