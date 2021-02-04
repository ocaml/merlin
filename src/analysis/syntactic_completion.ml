open Parser_raw

(** Generate syntactic completion by analysing the parser stack.
    The task is split in a few steps:

    1) First enumerate all reachable states by simulating all possible
       reductions.
       This is done by the Lookahead, Level and Stack modules. Lookahead keep
       tracks of a set of lookahead terminals: rather than simulating
       separately for each possible lookahead token, we regroup lookahead
       tokens that trigger the same reduction, such that a given reduction is
       simulated only once.
       Level keep track of all goto transitions to simulate on a given stack
       frame (hence the reductions are grouped per stack "level").
       Stack modules simulate the stack reduction.

       Auxiliary information are provided by:
       - Parser_complete.state_to_reduction_table: the list of reductions to
         simulate when in a given state, already structured per level
       - Parser_complete.state_goto_table: a naive but sufficient
         representation of the LR goto table.
         TODO: replace it by Menhir builtin goto table when possible (need to
               patch Menhir).

    2) After that, we have the list of all states that were reached, and for
       each state, the set of lookahead tokens that led to it.
       The list is ordered: the state that is the deepest in the stack comes
       first. This is done to favor completions that will close the most
       syntactic constructions over completions that might open new nested
       constructions.
       In practice, it means that in this example :
          module M = struct
            let v = if true then x
       Completing after the x, the suggestions will be:
       - first `end`, to close the structure
       - then `in`, to transforme the module let into an expression let
       - finally `else`, to turn the `if then` into an `if then else`.
       This order will be preserved by subsequent transformations.

       Then we turn each reached state into an "item set":

       - Parser_complete.state_closure_table associates to each state the
         states that can be reached by following "null" reductions.
         (e.g. if we are in `let . rec?` we can each `let rec? .` by assuming
          the rec flag is missing: rec? is a nullable reduction)
         TODO: maybe we can remove this step by simulating the closure from
               production definition, the runtime cost should be negligible.
       - Parser_complete.items_table associates a state to its itemset, in the
         form of a list of pair of (production, dot position)

    3) The item sets are transformed into sequence of symbols by looking
       them up in Parser_complete.productions table, that contains the
       definition of each production.
       Extra step: we need to simulate the "closure" of the itemset.
       For instance if we have an item that looks like `. expression`, we don't
       want to stop there and just suggest "expression", rather we want to
       expand the expression to its definition.
       This is done using Parser_complete.nonterminal_to_productions that lists
       all the productions that can produce a non-terminal.

    4) Now we have a list of symbols that constitutes valid continuations of
       the current parsing. We need to turn them into readable definitions that
       can be presented to the user.
       First, we keep only the ones that starts with tokens we consider
       "interesting" (mostly keywords) using "is_interesting_terminal".
       After that, for each starting terminal, we only keep the shortest
       sentence that can complete it. For instance,
         { if ... then ... , if ... then ... else ... ,
           let ... = ... , let ... = ... in ... }
       is simplified too
         { if ... then ... , let ... = ... }
       Then we turn terminals into text using Parser_printer and replace
       non-terminals by "..."

    [completion_for_parser] runs to whole pipeline.
*)

let terminal_index : _ MenhirInterpreter.terminal -> int = Obj.magic
let nonterminal_index : _ MenhirInterpreter.nonterminal -> int = Obj.magic

module Lookahead = struct
  type t = string
  (** A "lookahead" set is a set of terminals encoded as a string:
      each terminal as an index, the corresponding bit is set if the terminal
      is in the set or not.  *)

  let string_get s i = Char.code (String.get s i)
  let byte_get s i = Char.code (Bytes.get s i)
  let byte_set s i v = Bytes.set s i (Char.chr v)

  let union s1 s2 = match s1, s2 with
    | "", x | x, "" -> x
    | _ ->
      let s1, s2 =
        if String.length s1 > String.length s2 then s2, s1 else s1, s2
      in
      let result = Bytes.of_string s2 in
      for i = 0 to String.length s1 - 1 do
        byte_set result i (byte_get result i lor string_get s1 i)
      done;
      Bytes.unsafe_to_string result

  let inter s1 s2 =
    let len =
      let l = ref (min (String.length s1) (String.length s2)) in
      while !l > 0 && (string_get s1 !l land string_get s2 !l) = 0 do
        decr l
      done;
      !l
    in
    let result = Bytes.make len '\x00' in
    for i = 0 to len - 1 do
      byte_set result i (string_get s1 i land string_get s2 i)
    done;
    Bytes.unsafe_to_string result

  let mem mask terminal =
    mask = "" ||
    let index : int = terminal_index terminal in
    let offset = index / 8 in
    let shift = index mod 8 in
    if String.length mask > offset
    then Char.code mask.[offset] land (1 lsl shift) <> 0
    else false
end

module Level = struct
  type reduction = { goto: Parser_complete.production; lookahead: Lookahead.t }
  type level = reduction list
  type levels = level list

  let import_reduction (goto, lookahead) = {goto; lookahead}

  let rec merge_level lookahead = function
    | x, [] -> x
    | [], x -> List.map import_reduction x
    | (l1 :: ls1' as ls1), (l2 :: ls2' as ls2) ->
      if l1.goto < fst l2 then
        l1 :: merge_level lookahead (ls1', ls2)
      else if l1.goto > fst l2 then
        import_reduction l2 :: merge_level lookahead (ls1, ls2')
      else
        let lookahead =
          Lookahead.union l1.lookahead (Lookahead.inter lookahead (snd l2))
        in
        {goto = l1.goto; lookahead} :: merge_level lookahead (ls1', ls2')

  let rec merge_levels lookahead = function
    | x, [] -> x
    | [], x -> List.map (List.map import_reduction) x
    | (l1 :: t1), (l2 :: t2) ->
      merge_level lookahead (l1, l2) :: merge_levels lookahead (t1, t2)
end

module Stack = struct
  let expand_state state reached lookahead context =
    let reached = (state, lookahead) :: reached in
    let ctx = (Lazy.force Parser_complete.state_to_reduction_table).(state) in
    let context = Level.merge_levels lookahead (context, ctx) in
    (reached, context)

  let rec consume env reached = function
    | [] -> reached
    | [] :: context ->
      begin match MenhirInterpreter.pop env with
        | None -> reached
        | Some env' -> consume env' reached context
      end
    | ({Level. goto; lookahead} :: level') :: context' ->
      let current = MenhirInterpreter.current_state_number env in
      let state =
        List.assoc goto (Lazy.force Parser_complete.state_goto_table).(current)
      in
      let context = level' :: context' in
      let reached, context = expand_state state reached lookahead context in
      consume env reached context

  let analyse env : (int * string) list =
    let current = MenhirInterpreter.current_state_number env in
    let reached = [(current, "")] in
    match MenhirInterpreter.pop env with
    | None -> reached
    | Some env ->
      let levels =
        (Lazy.force Parser_complete.state_to_reduction_table).(current)
      in
      consume env reached (List.map (List.map Level.import_reduction) levels)
end

(** We don't want to display completions for all terminals: it makes sense to
    suggest introducing a keyword, but not an identifier. Similarly, most
    operators are better managed at the semantic layer, unless they have a very
    syntactic role (e.g. + is semantic while -> is syntactic). *)
let is_interesting_terminal : type a . a MenhirInterpreter.terminal -> bool =
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
  | T_LBRACKETAT             -> false
  | T_LBRACKETATAT           -> false
  | T_LBRACKETATATAT         -> false
  | T_LBRACKETBAR            -> true
  | T_LBRACKETGREATER        -> true
  | T_LBRACKETLESS           -> true
  | T_LBRACKETPERCENT        -> false
  | T_LBRACKETPERCENTPERCENT -> false
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

let rec expand_nt : type a.
  Lookahead.t -> int list ref ->
  MenhirInterpreter.xsymbol list list -> a MenhirInterpreter.nonterminal -> _ =
  fun lookahead expanded acc nt ->
  let nt = nonterminal_index nt in
  if List.mem nt !expanded then acc else (
    expanded := nt :: !expanded;
    List.fold_left begin fun acc prod ->
      let rhs = Parser_complete.productions.(prod) in
      let len = Array.length rhs in
      if len = 0 then acc else (
        match rhs.(0) with
        | X (N nt') -> expand_nt lookahead expanded acc nt'
        | X (T t) ->
          if is_interesting_terminal t && Lookahead.mem lookahead t
          then Array.to_list rhs :: acc
          else acc
      )
    end acc (Lazy.force Parser_complete.nonterminal_to_productions).(nt)
  )

let immediate_state_to_rhs lookahead acc state =
  let items = (Lazy.force Parser_complete.items_table).(state) in
  Array.fold_left begin fun acc (prod, dot) ->
    let rhs = Parser_complete.productions.(prod) in
    let len = Array.length rhs in
    if dot < len then
      match rhs.(dot) with
      | X (N n) -> expand_nt lookahead (ref []) acc n
      | X (T t) as sym ->
        if is_interesting_terminal t && Lookahead.mem lookahead t then
          let tail = Array.to_list (Array.sub rhs (dot + 1) (len - dot - 1)) in
          (sym :: tail) :: acc
        else acc
    else acc
  end acc items

let state_to_rhs (state, lookahead) =
  let states = (Lazy.force Parser_complete.state_closure_table).(state) in
  List.fold_left (immediate_state_to_rhs lookahead) [] (state :: states)

let states_to_rhs states =
  List.flatten (List.map state_to_rhs states)

let rhs_to_string rhs =
  let strings =
    List.map (fun (MenhirInterpreter.X x as sym) ->
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

let filter_rhs rhs =
  List.iter (fun rhs ->
      let a, b = rhs_to_string rhs in
      prerr_endline (a ^ " " ^ b)
    ) rhs;
  let terminal_table = Hashtbl.create 7 in
  let terminal_list = List.filter_map (function
      | (MenhirInterpreter.X (T t) :: _) as entry ->
        let index = terminal_index t in
        begin match Hashtbl.find terminal_table index with
        | exception Not_found ->
          Hashtbl.add terminal_table index entry;
          Some index
        | entry' ->
          if List.length entry < List.length entry' then
            Hashtbl.replace terminal_table index entry;
          None
        end
      | _ -> assert false
    ) rhs
  in
  List.map (Hashtbl.find terminal_table) terminal_list

let completion_for_parser env =
  env
  |> Stack.analyse
  |> states_to_rhs
  |> filter_rhs
  |> List.map rhs_to_string
