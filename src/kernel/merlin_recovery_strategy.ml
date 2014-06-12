open Std
open Raw_parser

let memoize n ~f =
  let f i = lazy (f i) in
  let cache = Array.init n f in
  fun i -> Lazy.force (cache.(i))

let measure_production prod =
  let def = Query.production_definition prod in
  try match Query.semantic_action prod with
    | None -> None
    | Some action ->
      let prepend_cost symclass (cost, values) =
        let cost', value = Raw_parser_values.default_symbol symclass in
        let cost = cost + cost' in
        let values = (cost, value) :: values in
        (cost, values)
      in
      let cost, values = List.fold_right ~f:prepend_cost def ~init:(0, []) in
      Some (values, prod, action)
  with Not_found -> None

let measure_production = memoize Query.productions ~f:measure_production

let reduction_strategy lr0 =
  let itemset = Raw_parser.Query.itemset lr0 in
  let measure_item (prod, pos) =
    match measure_production prod with
    (* items at pos 0 are forbidden: they won't consume anything on stack
       and as such can prevent termination *)
    | Some (values, prod, action) when pos > 0 ->
      let values = List.drop_n pos values in
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
        *)
        | 1 -> cost + 10
        (* In general we want to favor rightmost items *)
        | n -> cost - n * 2
      in
      Some (cost, values, prod, action)

    | _ -> None
  in
  let candidates = List.filter_map ~f:measure_item itemset in
  List.sort ~cmp:(fun (c1,_,_,_) (c2,_,_,_) -> compare c1 c2) candidates

let reduction_strategy = memoize Query.lr0_states ~f:reduction_strategy
