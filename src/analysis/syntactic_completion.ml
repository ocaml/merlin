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

let terminal_mem mask (index : _ Parser_raw.MenhirInterpreter.terminal) =
  mask = "" ||
  let index : int = Obj.magic index in
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

let interesting : type a . a Parser_raw.MenhirInterpreter.terminal -> bool =
  function
  | T_AMPERAMPER             -> false
  | T_AMPERSAND              -> false
  | T_AND                    -> true
  | T_ANDOP                  -> false
  | T_AS                     -> true
  | T_ASSERT                 -> true
  | T_BACKQUOTE              -> false
  | T_BANG                   -> false
  | T_BAR                    -> true
  | T_BARBAR                 -> false
  | T_BARRBRACKET            -> false
  | T_BEGIN                  -> true
  | T_CHAR                   -> false
  | T_CLASS                  -> true
  | T_COLON                  -> true
  | T_COLONCOLON             -> true
  | T_COLONEQUAL             -> true
  | T_COLONGREATER           -> true
  | T_COMMA                  -> true
  | T_COMMENT                -> false
  | T_CONSTRAINT             -> true
  | T_DO                     -> true
  | T_DOCSTRING              -> false
  | T_DONE                   -> true
  | T_DOT                    -> true
  | T_DOTDOT                 -> true
  | T_DOTLESS                -> false
  | T_DOTOP                  -> false
  | T_DOTTILDE               -> false
  | T_DOWNTO                 -> true
  | T_ELSE                   -> true
  | T_END                    -> true
  | T_EOF                    -> false
  | T_EOL                    -> false
  | T_EQUAL                  -> true
  | T_EXCEPTION              -> true
  | T_EXTERNAL               -> true
  | T_FALSE                  -> true
  | T_FINALLY_LWT            -> false
  | T_FLOAT                  -> false
  | T_FOR                    -> true
  | T_FOR_LWT                -> false
  | T_FUN                    -> true
  | T_FUNCTION               -> true
  | T_FUNCTOR                -> true
  | T_GREATER                -> false
  | T_GREATERDOT             -> false
  | T_GREATERRBRACE          -> true
  | T_GREATERRBRACKET        -> true
  | T_HASH                   -> false
  | T_HASHOP                 -> false
  | T_IF                     -> true
  | T_IN                     -> true
  | T_INCLUDE                -> true
  | T_INFIXOP0               -> false
  | T_INFIXOP1               -> false
  | T_INFIXOP2               -> false
  | T_INFIXOP3               -> false
  | T_INFIXOP4               -> false
  | T_INHERIT                -> true
  | T_INITIALIZER            -> true
  | T_INT                    -> false
  | T_LABEL                  -> false
  | T_LAZY                   -> true
  | T_LBRACE                 -> true
  | T_LBRACELESS             -> true
  | T_LBRACKET               -> true
  | T_LBRACKETAT             -> true
  | T_LBRACKETATAT           -> true
  | T_LBRACKETATATAT         -> true
  | T_LBRACKETBAR            -> true
  | T_LBRACKETGREATER        -> true
  | T_LBRACKETLESS           -> true
  | T_LBRACKETPERCENT        -> true
  | T_LBRACKETPERCENTPERCENT -> true
  | T_LESS                   -> false
  | T_LESSMINUS              -> true
  | T_LET                    -> true
  | T_LETOP                  -> false
  | T_LET_LWT                -> false
  | T_LIDENT                 -> false
  | T_LPAREN                 -> true
  | T_MATCH                  -> true
  | T_MATCH_LWT              -> false
  | T_METHOD                 -> true
  | T_MINUS                  -> false
  | T_MINUSDOT               -> false
  | T_MINUSGREATER           -> true
  | T_MODULE                 -> true
  | T_MUTABLE                -> true
  | T_NEW                    -> true
  | T_NONREC                 -> true
  | T_OBJECT                 -> true
  | T_OF                     -> true
  | T_OPEN                   -> true
  | T_OPTLABEL               -> false
  | T_OR                     -> false
  | T_PERCENT                -> false
  | T_PLUS                   -> false
  | T_PLUSDOT                -> false
  | T_PLUSEQ                 -> false
  | T_PREFIXOP               -> false
  | T_PRIVATE                -> true
  | T_QUESTION               -> false
  | T_QUESTIONQUESTION       -> false
  | T_QUOTE                  -> false
  | T_QUOTED_STRING_EXPR     -> false
  | T_QUOTED_STRING_ITEM     -> false
  | T_RBRACE                 -> true
  | T_RBRACKET               -> true
  | T_REC                    -> true
  | T_RPAREN                 -> true
  | T_SEMI                   -> true
  | T_SEMISEMI               -> true
  | T_SIG                    -> true
  | T_SNAPSHOT               -> false
  | T_STAR                   -> false
  | T_STRING                 -> false
  | T_STRUCT                 -> true
  | T_THEN                   -> true
  | T_TILDE                  -> false
  | T_TO                     -> true
  | T_TRUE                   -> true
  | T_TRY                    -> true
  | T_TRY_LWT                -> false
  | T_TYPE                   -> true
  | T_UIDENT                 -> false
  | T_UNDERSCORE             -> true
  | T_VAL                    -> true
  | T_VIRTUAL                -> true
  | T_WHEN                   -> true
  | T_WHILE                  -> true
  | T_WHILE_LWT              -> false
  | T_WITH                   -> true
  | T_error                  -> false

let rec expand_nt lookahead expanded acc tail nt =
  if List.mem nt !expanded then acc else (
    expanded := nt :: !expanded;
    let prods = (Lazy.force Complete_data.nonterminal_prods).(nt) in
    List.fold_left (fun acc prod ->
        let rhs = Complete_data.productions.(prod) in
        let len = Array.length rhs in
        if len = 0 then acc else (
          let tail = Array.to_list (Array.sub rhs 1 (len - 1)) @ tail in
          match rhs.(0) with
          | X (N nt') ->
            expand_nt lookahead expanded acc tail (Obj.magic nt' : int)
          | X (T t) as sym ->
            if interesting t && terminal_mem lookahead t
            then (sym :: tail) :: acc
            else acc
        )
      ) acc prods
  )

let immediate_state_to_rhs lookahead acc state =
  let items = (Lazy.force Complete_data.items_table).(state) in
  Array.fold_left begin fun acc (prod, dot) ->
    let rhs = Complete_data.productions.(prod) in
    let len = Array.length rhs in
    if dot < len then
      let tail = Array.to_list (Array.sub rhs (dot + 1) (len - dot - 1)) in
      match rhs.(dot) with
      | X (N n) -> expand_nt lookahead (ref []) acc tail (Obj.magic n)
      | X (T t) as sym ->
        if interesting t && terminal_mem lookahead t
        then (sym :: tail) :: acc
        else acc
    else acc
  end acc items

let state_to_rhs (state, lookahead) =
  let lazy reds = Complete_data.reduction_table in
  let _, states = reds.(state) in
  List.fold_left (immediate_state_to_rhs lookahead) [] (state :: states)
  |> List.sort (fun l1 l2 -> Int.compare (List.length l1) (List.length l2))

let rhs_to_string rhs =
  let strings =
    List.map (fun (Parser_raw.MenhirInterpreter.X x as sym) ->
      match x with
      | N _ -> "..."
      | _ -> Parser_printer.print_symbol sym
    ) rhs
  in
  let rec dedup = function
    | [] -> []
    | "..." :: ("..." :: _ as rest) -> dedup rest
    | x :: xs -> x :: dedup xs
  in
  match dedup strings with
  | [] -> assert false
  | hd :: tl ->
    hd, String.concat " " tl
