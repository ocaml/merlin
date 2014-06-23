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

type measurement =
  {
    (* If a production is a of the form "<nt1>: <nt1> …",
       then we forbid reducing it if only <nt1> has been given, since
       this can cause infinite loops. *)
    m_left_rec: bool;

    m_rhs: (cost * symbol) list;

    m_action: Query.semantic_action;
  }

let measure_production prod =
  match Query.production_definition prod with
  | _, (CT_ (T_ENTRYPOINT, _) :: _) -> None
  | lhs, rhs ->
    try match Query.semantic_action prod with
    | None -> None
    | Some m_action ->
      let prepend_cost symclass (cost, values) =
        let cost', value = Raw_parser_values.default_symbol symclass in
        let cost = cost + cost' + 1 in
        let values = (cost, value) :: values in
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

let can_use m_left_rec pos =
  match pos with
  | 0 -> false
  | 1 -> not m_left_rec
  | n -> assert (n > 0); true

type strategy = {
  uid: int;
  mutable cost: cost;
  first: bool;

  symbols: symbol list;
  prod: int;
  action: Query.semantic_action;
}

let genuid =
  let k = ref 0 in
  fun () -> incr k; !k

let reduction_strategy lr0 =
  let itemset = Raw_parser.Query.itemset lr0 in
  let measure_item (prod, pos) =
    match measure_production prod with
    (* items at pos 0 are forbidden: they won't consume anything on stack
       and as such can prevent termination *)
    | Some {m_rhs; m_left_rec; m_action} when can_use m_left_rec pos ->
      let values = List.drop_n pos m_rhs in
      let cost = match values with
        | (cost, _) :: _ -> cost
        | [] -> 0
      in
      let symbols = List.map ~f:snd values in
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
      Some {uid = genuid (); cost; first = pos = 1; symbols; prod; action = m_action}

    | _ -> None
  in
  let candidates = List.filter_map ~f:measure_item itemset in
  List.sort ~cmp:(fun s1 s2 -> compare s1.cost s2.cost) candidates

let reduction_strategy =
  memoize Query.lr0_states
    ~f:reduction_strategy
    ~check:(function {cost = -1} :: tl -> Some tl
                   | _ -> None)

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

