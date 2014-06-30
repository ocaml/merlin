open Std
open Raw_parser

let memoize ?check n ~f =
  let f i = lazy (f i) in
  let cache = Array.init n f in
  match check with
  | None -> (fun i -> Lazy.force cache.(i))
  | Some check ->
    (fun i ->
      let cell = Lazy.force cache.(i) in
      match check cell with
      | None -> cell
      | Some cell' ->
        cache.(i) <- lazy cell';
        cell')

type cost = int

let rec annotcost = function
  | `Cost n :: xs -> n + annotcost xs
  | _ :: xs -> annotcost xs
  | [] -> 0

let rec annotindent = function
  | `Indent n :: xs -> n + annotcost xs
  | _ :: xs -> annotcost xs
  | [] -> 0

type measurement =
  {
    (* If a production is a of the form "<nt1>: <nt1> …",
       then we forbid reducing it if only <nt1> has been given, since
       this can cause infinite loops. *)
    m_left_rec: bool;

    m_rhs: (cost * annotation list * symbol) list;

    m_action: Query.semantic_action;
  }

let measure_production prod =
  match Query.production_definition prod with
  | _, (CT_ (T_ENTRYPOINT, _) :: _) -> None
  | lhs, rhs ->
    try match fst (Query.semantic_action prod) with
    | None -> None
    | Some m_action ->
      let prepend_cost symclass (cost, values) =
        let annot = match symclass with
          | CT_ (_,annot) -> annot
          | CN_ (_,annot) -> annot
        in
        let cost', value = Raw_parser_values.default_symbol symclass in
        let cost = cost + cost' + annotcost annot + 1 in
        let values = (cost, annot, value) :: values in
        (cost, values)
      in
      let m_left_rec = match lhs, rhs with
        | Some sym, (sym' :: _) -> sym = sym'
        | _ -> false
      in
      let _cost, m_rhs = List.fold_right ~f:prepend_cost rhs ~init:(0, []) in
      Some { m_left_rec ; m_rhs; m_action }
    with Not_found -> None

let measure_production = memoize Query.productions ~f:measure_production

let can_use {m_left_rec} pos =
  match pos with
  | 0 -> false
  | 1 -> not m_left_rec
  | n -> assert (n > 0); true

type reduction = {
  r_symbols: symbol list;
  r_prod: int;
  r_action: Query.semantic_action;
}

type strategy = {
  uid: int;
  mutable cost: cost;
  first: bool;
  action: [`Reduce of reduction | `Shift of (int * token * int)];
  reindent: int;
}

let genuid =
  let k = ref 0 in
  fun () -> incr k; !k

let reduction_strategy lr0 =
  let itemset = Raw_parser.Query.itemset lr0 in
  let make_reduction {m_rhs; m_left_rec; m_action} prod pos cost values =
    let r_symbols = List.map ~f:Misc.thd3 values in
    let cost = match pos with
      (* items at pos 1 will produce one new frame and consume one frame.
           as such, they can cause loop too but might sometimes be needed.
           FIXME: We might need to provide a finer metric to prevent a loop
           between 1-items.

           A first step to do so would be to forbid 1-item reducing to
           the same non-terminal as their lhs. (DONE)
           Generalizing on this idea, we should forbid 1-item reducing to a
           cycle in their lhs.  *)
      | 1 -> cost + 10
      (* In general we want to favor rightmost items *)
      | n -> cost - n * 2 + (if m_left_rec then 10 else 0)
    in
    cost, `Reduce {r_symbols; r_prod = prod; r_action = m_action}
  in
  let measure_item (prod, pos) =
    match measure_production prod with
    (* items at pos 0 are forbidden: they won't consume anything on stack
       and as such can prevent termination *)
    | Some measurement when can_use measurement pos ->
      let values = measurement.m_rhs in
      let annot, values =
        if pos > 0 then
          match List.drop_n (pos - 1) values  with
          | (_, annot, _) :: values -> annot, values
          | _ -> assert false
        else
          [], values
      in
      let reindent = annotindent annot in
      let cost = match values with
        | (cost, _, _) :: _ -> cost
        | [] -> 0
      in
      let cost, action =
        match annot with
        | [`Shift_token (n,token)] ->
          cost - 10, `Shift (pos, token, n)
        | _ ->
        match values with
        | (_, [`Shift n], T_ (toksym,value)) :: _ ->
          cost - 10, `Shift (pos, Raw_parser_values.token_of_symbol toksym value, n)
        | _ -> make_reduction measurement prod pos cost values
      in
      Some {uid = genuid (); cost; first = pos = 1; action; reindent }
    | _ -> None
  in
  let candidates = List.filter_map ~f:measure_item itemset in
  let reindent = List.fold_left ~init:0 ~f:(fun r c -> min r c.reindent) candidates in
  let candidates = List.map ~f:(fun c -> {c with reindent}) candidates in
  List.sort ~cmp:(fun s1 s2 -> compare s1.cost s2.cost) candidates

let reduction_strategy =
  memoize Query.lr0_states
    ~f:reduction_strategy
    ~check:(function {cost = -1} :: tl -> Some tl
                   | _ -> None)

let parser_pos lr0 =
  match List.map ~f:snd (Raw_parser.Query.itemset lr0) with
  | [] -> 0
  | x :: xs -> List.fold_left ~f:min ~init:x xs
let parser_pos = memoize Query.lr0_states ~f:parser_pos


module Termination : sig
  type 'a t
  val initial: 'a t
  val check: strategy -> 'a -> 'a t -> 'a * 'a t * bool
end = struct

  module IntMap = Map.Make(struct
      type t = int
      let compare : int -> int -> int = compare
    end)

  type 'a t = 'a cell IntMap.t
  and 'a cell = Cell of 'a * 'a t

  let initial = IntMap.empty

  let check strat a m =
    if not strat.first then
      a, initial, true
    else try
      let Cell (a, m) = IntMap.find strat.uid m in
      strat.cost <- -1; (* Ban strategy *)
      a, m, false
    with Not_found ->
      a, IntMap.add strat.uid (Cell (a,m)) m, true

end

