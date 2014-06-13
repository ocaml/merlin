open Std
open Raw_parser

let memoize n ~f =
  let f i = lazy (f i) in
  let cache = Array.init n f in
  fun i -> Lazy.force (cache.(i))

type measurement =
  {
    (* If a production is a of the form "<nt1>: <nt1> …",
       then we forbid reducing it if only <nt1> has been given, since
       this can cause infinite loops. *)
    left_recursive: bool;

    rhs: (int * symbol) list;

    production: int;
    action: Query.semantic_action;
  }

let measure_production prod =
  let lhs, rhs = Query.production_definition prod in
  try match Query.semantic_action prod with
    | None -> None
    | Some action ->
      let prepend_cost symclass (cost, values) =
        let cost', value = Raw_parser_values.default_symbol symclass in
        let cost = cost + cost' + 1 in
        let values = (cost, value) :: values in
        (cost, values)
      in
      let left_recursive = match lhs, rhs with
        | Some sym, (sym' :: _) -> sym = sym'
        | _ -> false
      in
      let _cost, values = List.fold_right ~f:prepend_cost rhs ~init:(0, []) in
      Some { left_recursive ; rhs = values; production = prod; action }
  with Not_found -> None

let measure_production = memoize Query.productions ~f:measure_production

let can_use measurement pos =
  match pos with
  | 0 -> false
  | 1 -> not measurement.left_recursive
  | n -> assert (n > 0); true

let reduction_strategy lr0 =
  let itemset = Raw_parser.Query.itemset lr0 in
  let measure_item (prod, pos) =
    match measure_production prod with
    (* items at pos 0 are forbidden: they won't consume anything on stack
       and as such can prevent termination *)
    | Some measurement when can_use measurement pos ->
      let values = List.drop_n pos measurement.rhs in
      let cost = match values with
        | (cost, _) :: _ -> cost
        | [] -> 0
      in
      let values = List.map ~f:snd values in
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
        | n -> cost - n * 2 + (if measurement.left_recursive then 10 else 0)
      in
      Some (cost, values, prod, measurement.action)

    | _ -> None
  in
  let candidates = List.filter_map ~f:measure_item itemset in
  List.sort ~cmp:(fun (c1,_,_,_) (c2,_,_,_) -> compare c1 c2) candidates

let reduction_strategy = memoize Query.lr0_states ~f:reduction_strategy
