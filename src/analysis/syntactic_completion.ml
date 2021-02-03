let byte s i = Char.code (String.get s i)
let byte' s i = Char.code (Bytes.get s i)
let byte_set s i v = Bytes.set s i (Char.chr v)

let union_mask lookahead s1 s2 =
  let len1 = String.length s1 in
  let len2 =
    let l = ref (min (String.length lookahead) (String.length s2)) in
    while !l > 0 && (byte lookahead !l land byte s2 !l) = 0 do
      decr l
    done;
    !l
  in
  let result = Bytes.make (max len1 len2) '\x00' in
  for i = 0 to len1 - 1 do
    byte_set result i (byte s1 i)
  done;
  for i = 0 to len2 - 1 do
    byte_set result i
      (byte' result i lor (byte lookahead i land byte s2 i))
  done;
  Bytes.unsafe_to_string result

let mask_mem mask index =
  let offset = index / 8 in
  let shift = index mod 8 in
  if String.length mask > offset
  then Char.code mask.[offset] land (1 lsl shift) <> 0
  else false

let rec merge_level lookahead = function
  | [], x | x, [] -> x
  | ((x1, s1 as h1) :: t1 as ht1), ((x2, s2 as h2) :: t2 as ht2) ->
    if x1 < x2 then
      h1 :: merge_level lookahead (t1, ht2)
    else if x1 > x2 then
      h2 :: merge_level lookahead (ht1, t2)
    else
      (x1, union_mask lookahead s1 s2) :: merge_level lookahead (t1, t2)

let rec augment_context lookahead = function
  | [], x | x, [] -> x
  | (l1 :: t1), (l2 :: t2) ->
    merge_level lookahead (l1, l2) :: augment_context lookahead (t1, t2)

let expand_state state reached lookahead context =
  let reached = (state, lookahead) :: reached in
  let context', _ = (Lazy.force Complete_data.reduction_table).(state) in
  let context = augment_context lookahead (context, context') in
  (reached, context)

let rec consume_stack env reached = function
  | [] -> reached
  | [] :: context ->
    begin match Parser_raw.MenhirInterpreter.pop env with
      | None -> reached
      | Some env' -> consume_stack env' reached context
    end
  | ((goto, lookahead) :: level') :: context' ->
    let current = Parser_raw.MenhirInterpreter.current_state_number env in
    let state =
      let lazy goto_table = Complete_data.goto_table in
      List.assoc goto goto_table.(current)
    in
    let context = level' :: context' in
    let reached, context = expand_state state reached lookahead context in
    consume_stack env reached context

let analyse_stack env : (int * string) list =
  let current = Parser_raw.MenhirInterpreter.current_state_number env in
  let reached = [(current, "")] in
  let context, _ = (Lazy.force Complete_data.reduction_table).(current) in
  match Parser_raw.MenhirInterpreter.pop env with
  | None -> reached
  | Some env -> consume_stack env reached context

let state_to_rhs (state, lookahead) =
  let lazy items_table = Complete_data.items_table in
  Array.fold_left begin fun acc (prod, dot) ->
    let rhs = Complete_data.productions.(prod) in
    let len = Array.length rhs in
    if dot < len then
      match rhs.(dot) with
      | X (N _) -> acc
      | X (T t) ->
        let index : int = Obj.magic t in
        if lookahead = "" || mask_mem lookahead index then
          Array.sub rhs dot (len - dot) :: acc
        else
          acc
    else acc
  end [] items_table.(state)

let rhs_to_string rhs =
  match Array.to_list (Array.map Parser_printer.print_symbol rhs) with
  | [] -> assert false
  | hd :: tl ->
    hd, String.concat " " tl
