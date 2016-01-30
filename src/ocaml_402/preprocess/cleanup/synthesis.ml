open MenhirSdk.Cmly_format
open Utils

type variable =
  | Head of lr1_state * nonterminal
  | Tail of lr1_state * production * int

type 'a paction =
  | Abort
  | Reduce of production
  | Shift  of symbol
  | Var    of 'a

type action = variable paction

module type Grammar = sig
  val grammar         : grammar
  val cost_of_prod    : production -> float
  val penalty_of_item : production * int -> float
  val cost_of_symbol  : symbol -> float
end

module type Solution = sig
  val grammar  : grammar
  val cost_of  : variable -> float
  val cost_of_action  : action -> float
  val cost_of_actions : action list -> float
  val solution : variable -> action list
  val report   : Format.formatter -> unit
end

module Make (G : Grammar) : Solution = struct
  open G

  let grammar = grammar

  let app var v = v var

  let var var = match var with
    | Head _ -> app var
    | Tail (_,prod,pos) ->
        if pos < Array.length prod.p_rhs then
          app var
        else
          let cost = cost_of_prod prod in
          const cost

  let cost_of = function
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
                (*report "no transition: #%d (%d,%d)\n" st.lr1_index prod.p_index pos;*)
                const infinity
          in
          (fun v -> head v +. tail v)

  let cost_of =
    let module Solver = MenhirSdk.Fix.Make (struct
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
    in
    Solver.lfp cost_of

  let cost_of_action = function
    | Abort    -> infinity
    | Reduce p -> cost_of_prod p
    | Shift s  -> cost_of_symbol s
    | Var v    -> cost_of v

  let select var1 var2 =
    arg_min_float cost_of_action var1 var2

  let solution = function
    | Head (st, n) ->
        let acc = Abort in
        let acc =
          Array.fold_left
            (fun acc (sym, st') ->
               Array.fold_left (fun acc (prod, pos) ->
                   if pos = 1 && prod.p_lhs = n then
                     select (Var (Tail (st, prod, 0))) acc
                   else acc
                 ) acc st'.lr1_lr0.lr0_items
            ) acc st.lr1_transitions
        in
        let acc =
          Array.fold_left
            (fun acc (_, prods) ->
               List.fold_left (fun acc prod ->
                   if prod.p_rhs = [||] && prod.p_lhs = n then
                     select (Reduce prod) acc
                   else acc
                 ) acc prods
            ) acc st.lr1_reductions
        in
        [acc]

    | Tail (st, prod, pos) when pos = Array.length prod.p_rhs ->
        [Reduce prod]

    | Tail (st, prod, pos) ->
        let penalty = penalty_of_item (prod, pos) in
        if penalty = infinity then
          [Abort]
        else
          let head =
            let sym, _, _ = prod.p_rhs.(pos) in
            let cost = cost_of_symbol sym in
            if cost < infinity then
              Shift sym
            else match sym with
              | T _ -> Abort
              | N n -> Var (Head (st, n))
          in
          let tail =
            match array_assoc st.lr1_transitions (fst3 prod.p_rhs.(pos)) with
            | st' -> Var (Tail (st', prod, pos + 1))
            | exception Not_found ->
                Abort
          in
          [head; tail]

  let cost_of_actions actions =
    List.fold_left (fun cost act -> cost +. cost_of_action act) 0.0 actions

  let report ppf =
    let open Format in
    let solutions =
      Array.fold_left (fun acc st ->
          match Array.fold_left (fun (item, cost) (prod, pos) ->
              let cost' = cost_of (Tail (st, prod, pos)) in
              assert (cost' = cost_of_actions (solution (Tail (st, prod, pos))));
              if cost' < cost then (Some (prod, pos), cost') else (item, cost)
            ) (None, infinity) st.lr1_lr0.lr0_items
          with
          | None, _ ->
              fprintf ppf "no synthesis from %d\n" st.lr1_index;
              acc
          | Some item, cost -> (item, (cost, st.lr1_index)) :: acc
        ) [] grammar.g_lr1_states
    in
    List.iter (fun (item, states) ->
        fprintf ppf "# Item (%d,%d)\n" (fst item).p_index (snd item);
        print_table ppf (items_table [item]);
        List.iter (fun (cost, states) ->
            fprintf ppf "at cost %f from states %s\n\n"
              cost (list_fmt string_of_int states)
          ) (group_assoc states)
      ) (group_assoc solutions)
end
