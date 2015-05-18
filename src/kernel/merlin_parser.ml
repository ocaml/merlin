(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2014  Frédéric Bour  <frederic.bour(_)lakaban.net>
                             Thomas Refis  <refis.thomas(_)gmail.com>
                             Simon Castellan  <simon.castellan(_)iuwt.fr>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

open Std

module Values = Raw_parser_values

module P = Raw_parser
module E = MenhirLib.EngineTypes

let section = Logger.section "parser"

type state = Raw_parser.state

type t = P.feed P.parser
type frame = (P.state, P.symbol) E.stack

let stack s = s.P.env.E.stack
let get_lr1_state s = s.P.env.E.current
let get_lr0_state s = P.Query.lr0_state (get_lr1_state s)

module Frame : sig
  val value : frame -> P.symbol
  val location : ?pop:int -> frame -> Location.t
  val eq    : frame -> frame -> bool
  val next  : ?n:int -> frame -> frame option

  val lr1_state : frame -> int
  val lr0_state : frame -> int
end = struct

  let frame_of = function
    | stack when stack.E.next == stack -> None
    | stack -> Some stack

  let value frame = frame.E.semv

  let pop n f =
    assert (n >= 0);
    let rec loop f = function
      | 0 -> f
      | n -> loop f.E.next (n - 1)
    in
    loop f n

  let location ?pop:(n=0) frame =
    let frame = pop n frame in
    { Location.
      loc_start = frame.E.startp;
      loc_end   = frame.E.endp;
      loc_ghost = false }

  let next ?(n=1) f = frame_of (pop n f)

  let eq f f' = f == f'

  let lr1_state stack = stack.E.state
  let lr0_state stack = P.Query.lr0_state (lr1_state stack)
end

let implementation = P.implementation_state
let interface = P.interface_state

let pop p =
  match MenhirUtils.pop p.P.env with
  | None -> None
  | Some env -> Some {p with P.env = env}

let rec get_guide col pop t =
  let col = min col (Lexing.column (Frame.location ~pop t).Location.loc_start) in
  if pop <= 0 then
    col
  else
    get_guide col (pop - 1) t

let get_guide ~pop t =
  get_guide max_int pop (stack t)

let get_location ?pop t =
  match pop with
  | None ->
    let pop = Merlin_recovery_strategy.parser_pos (get_lr0_state t) in
    let col = get_guide pop t in
    let loc = Frame.location ~pop (stack t) in
    {loc with Location.loc_start = Lexing.set_column loc.Location.loc_start col}
  | Some pop -> Frame.location ~pop (stack t)

let rec of_step s =
  match P.step s with
  | `Accept _ as result -> result
  | `Reject p ->
    `Reject (Obj.magic p : t)
  | `Feed p -> `Step p
  | `Step p -> of_step p

let from state input =
  match of_step (P.initial state input) with
  | `Step p -> p
  | _ -> assert false

let feed (s,t,e as input) ?(record_comment=ignore) parser =
  match t with
  (* Ignore comments *)
  | P.COMMENT c ->
    record_comment c ;
    `Step parser
  | _ -> of_step (P.feed parser input)

let dump_item (prod, dot_pos) =
  let lhs, rhs = P.Query.production_definition prod in
  let lhs = Option.value_map ~f:Values.string_of_class ~default:"?" lhs in
  let rhs = List.map ~f:Values.string_of_class rhs in
  let prefix, suffix = List.split_n dot_pos rhs in
  `Assoc [
    "item", `List [`Int prod; `Int dot_pos];
    "non_terminal", `String lhs;
    "definition", `String (String.concat " " (prefix @ ["."] @ suffix));
  ]

let dump_itemset l =
  `List (List.map dump_item l)

let dump_frame frame =
  let v = Frame.value frame in
  let position = (Frame.location frame).Location.loc_start in
  `Assoc [
    "position", Lexing.json_of_position position;
    "content", `String (Values.(string_of_class (class_of_symbol v)));
  ]

let rec dump_stack acc = function
  | None -> `List (List.rev acc)
  | Some frame ->
    dump_stack ((dump_frame frame) :: acc) (Frame.next frame)
let dump_stack xs = dump_stack [] xs

let dump t =
  (* Print current frame, with its itemset *)
  let lr0 = get_lr0_state t in
  (* Print overview of the stack *)
  `Assoc [
    "guide", Lexing.json_of_position (get_location t).Location.loc_start;
    "lr0", `Int lr0;
    "itemset", dump_itemset (P.Query.itemset lr0);
    "stack", dump_stack (Some (stack t));
  ]

let dump_strategy {Merlin_recovery_strategy. cost; action} =
  `Assoc [
    "cost", `Int cost;
    "action",
    begin match action with
    | `Reduce {Merlin_recovery_strategy. r_prod; r_symbols = l} ->
      let l = List.map ~f:Values.class_of_symbol l in
      let l = List.map ~f:Values.string_of_class l in
      let l = List.map ~f:Json.string l in
      (* FUCK FUCK FUCK JSON, NO WAY TO REPRESENT SUMS ?!
         HOW IS IT POSSIBLE TO DESIGN SUCH SHIT *)
      `List [`String "reduce"; `Assoc [
               "production", `Int r_prod;
               "symbols", `List l;
             ] ]
    | `Shift (pop, token, priority) ->
      let token = Values.symbol_of_token token in
      let token = Values.class_of_symbol token in
      let token = Values.string_of_class token in
      `List [`String "shift"; `Assoc [
               "dot_pos", `Int pop;
               "token", `String token;
               "priority", `Int priority;
             ] ]
    end
  ]

let dump_strategies (lr0,strategies) =
  `Assoc [
    "lr0", `Int lr0;
    "itemset", dump_itemset (P.Query.itemset lr0);
    "strategies", `List (List.map ~f:dump_strategy strategies)
  ]

let find_strategies p =
  let lr0 = get_lr0_state p in
  let strategies = Merlin_recovery_strategy.reduction_strategy lr0 in
  Logger.infojf section ~title:"find_strategies" dump_strategies
    (lr0,strategies);
  strategies

let last_token raw_parser =
  let loc_start,t,loc_end = raw_parser.P.env.E.token in
  Location.mkloc t
    {Location. loc_start; loc_end; loc_ghost = false}

type termination = t Merlin_recovery_strategy.Termination.t
let termination = Merlin_recovery_strategy.Termination.initial

let parser_priority p =
  let symbol = Frame.value (stack p) in
  let symcls = Values.class_of_symbol symbol in
  Values.selection_priority symcls

let rec recover ?endp termination parser =
  let open Merlin_recovery_strategy in
  match find_strategies parser with
  | [] -> None
  | strat :: _ ->
  match Termination.check strat parser termination with
  | parser, termination, false ->
    recover ?endp termination parser
  | parser, termination, true ->
    (* Feed stack *)
    match strat.action with
    | `Reduce {r_prod; r_symbols; r_action} ->
      let env = parser.P.env in
      let add_symbol endp stack symbol =
        {stack with E. semv = symbol; startp = stack.E.endp; endp; next = stack}
      in
      let add_symbol = match endp with
        | Some endp -> add_symbol endp
        | None -> (fun stack -> add_symbol stack.E.endp stack)
      in
      let stack = List.fold_left ~f:add_symbol ~init:env.E.stack r_symbols in
      let env = {env with E. stack} in
      (* Reduce stack *)
      (* FIXME: action can raise an error. We should catch it and fallback to
         another strategy *)
      let stack = r_action env in
      let env = {env with E. stack} in

      (* Follow goto transition *)
      (* FIXME: Rework menhir interface to expose appopriate primitives *)
      let module M = MenhirLib in
      let module T = P.MenhirInterpreterTable in
      let unmarshal2 table i j =
        M.RowDisplacement.getget
          M.PackedIntArray.get
          M.PackedIntArray.get
          table
          i j
      in
      let goto state prod =
        let code = unmarshal2 T.goto state (M.PackedIntArray.get T.lhs prod) in
        (* code = 1 + state *)
        code - 1
      in
      let env = {env with E. current = goto stack.E.state r_prod} in

      (* Construct parser *)
      let parser = {parser with P. env} in
      let priority = parser_priority parser in
      let parser = Location.mkloc parser (get_location parser) in
      Some (termination, (priority, parser))

    | `Shift (pop,token,priority) ->
      let loc = get_location parser in
      let token =
        let startp = loc.Location.loc_end in
        let endp = Option.value ~default:startp endp in
        (startp,token,endp)
      in
      let loc = Parsing_aux.location_union (get_location ~pop parser) loc in
      match feed token parser with
      | `Accept _ | `Reject _ -> None
      | `Step parser ->
        let parser = Location.mkloc parser loc in
        Some (termination, (priority, parser))

let get_lr0_states parser =
  List.Lazy.Cons (get_lr0_state parser, lazy begin
      let frames = List.Lazy.unfold Frame.next (stack parser) in
      List.Lazy.map Frame.lr0_state frames
    end)

let get_lr1_states parser =
  List.Lazy.Cons (get_lr1_state parser, lazy begin
      let frames = List.Lazy.unfold Frame.next (stack parser) in
      List.Lazy.map Frame.lr1_state frames
    end)

let find_marker t =
  (* FIXME: rather than hardcoded heuristic to find marker, annotate the
     grammar to know which states can serve as markers *)
  let frame = stack t in
  if Frame.value frame = P.T_ (P.T_ENTRYPOINT, ()) then
    (* No marker in empty parser *)
    None
  else
    (* Searching for marker position is split in two steps.
       First, finds the top marker: last state of the stack that belongs to
       current definition, so that when the marker is removed, current definition
       is complete. *)
    let end_top frame = match Frame.value frame with
      | P.N_ (P.N_structure_item, _) -> true
      | P.N_ (P.N_structure, _) -> true
      | P.N_ (P.N_signature_item, _) -> true
      | P.N_ (P.N_signature, _) -> true
      | P.T_ (P.T_SEMISEMI, ()) -> true
      | P.N_ (P.N_toplevel_directives, ()) -> true
      | P.T_ (P.T_ENTRYPOINT, ()) -> true
      | _ -> false
    in
    let rec find_top acc = function
      | None -> acc, None
      | Some frame when end_top frame -> acc, (Frame.next frame)
      | Some frame -> find_top frame (Frame.next frame)
    in
    let marker, rest = find_top frame (Some frame) in
    (* Second step: we look for recursive definitions in the rest of the stack.
       If there are some, marker should be put at beginning of the first one. *)
    let if_rec frame acc = match Frame.value frame with
      | P.T_ (P.T_REC, ()) -> frame
      | P.N_ (P.N_rec_flag, Asttypes.Recursive) -> frame
      | P.N_ (P.N_class_fields, _) -> frame
      | P.N_ (P.N_class_declarations, _) -> frame
      | P.N_ (P.N_class_descriptions, _) -> frame
      | P.N_ (P.N_module_rec_declarations, _) -> frame
      | _ -> acc
    in
    let rec find_rec acc = function
      | None -> acc
      | Some frame -> find_rec (if_rec frame acc) (Frame.next frame)
    in
    let marker = find_rec marker rest in
    Some marker

let rec next_s s =
  if s.E.startp == Lexing.dummy_pos then
    match s with
    | {E.next} when s == next ->
      raise Not_found
    | {E.next} -> next_s next
  else s

let next_s {E.next = s} =
  if s.E.startp == Lexing.dummy_pos
  then next_s s
  else s

let rec root_frame s1 s2 =
  if s1 == s2 then s1
  else
    let c = Lexing.compare_pos s1.E.startp s2.E.startp in
    if c > 0 then
      root_frame (next_s s1) s2
    else if c < 0 then
      root_frame s1 (next_s s2)
    else
      root_frame (next_s s1) (next_s s2)

let has_marker t f' =
  let rec aux = function
    | {E.next} as f when f == next -> false
    | f when Frame.eq f f' -> true
    | f when Lexing.compare_pos f.E.startp f'.E.startp < 0 -> false
    | {E.next} -> aux next
  in
  aux (stack t)

let has_marker ?diff t f' =
  match diff with
  | None -> has_marker t f'
  | Some (t',result) ->
    let s, s' = stack t, stack t' in
    try
      let p = root_frame s s' in
      if Lexing.compare_pos p.E.startp f'.E.startp > 0 then
        result
      else
        raise Not_found
    with Not_found -> has_marker t f'

let rec unroll_stack acc s' s =
  if s == s' then acc
  else
    let {E.next} = s in
    if next == s then
      (* [s'] is not an ancestor *)
      invalid_arg "unroll_stack"
    else
      unroll_stack (s :: acc) s' next

let unroll_stack ~from ~root = unroll_stack [] root from
