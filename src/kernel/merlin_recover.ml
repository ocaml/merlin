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
open Raw_parser

let section = Logger.section "recover"

let candidate_pos (_,{Location.txt = _; loc}) =
  Lexing.split_pos loc.Location.loc_start

let rollbacks endp parser =
  let locate parser =
    let loc = Merlin_parser.get_location parser in
    Location.mkloc parser loc
  in
  let rec aux (termination,(_,{Location.txt = parser})) =
    (* FIXME: find proper way to handle limit conditions *)
    (* When reaching bottom of the stack, last frame will raise an Accept
       exception, we can't recover from it, and we shouldn't have to recover up
       TO it. *)
    try
      match Merlin_parser.recover ~endp termination parser with
      | Some _ as r -> r
      | None ->
        let rec pop = function
          | None -> None
          | Some parser ->
            if Merlin_recovery_strategy.observable_state
                (Merlin_parser.get_lr1_state parser)
            then Some parser
            else pop (Merlin_parser.pop parser)
        in
        Option.map (pop (Merlin_parser.pop parser)) ~f:(fun parser ->
          Merlin_parser.termination, (0, locate parser))
    with _ -> None
  in
  let parser = Merlin_parser.termination, (0, locate parser) in
  let stacks = List.unfold aux parser in
  let stacks = List.rev_map stacks ~f:snd in
  (* Hack to drop last parser *)
  let stacks = List.sort (fun c1 c2 ->
      let _, col1 = candidate_pos c1 in
      let _, col2 = candidate_pos c2 in
      - compare col1 col2)
      stacks
  in
  Zipper.of_list stacks

type t = {
  errors    : exn list;
  comments  : (string * Location.t) list ;
  parser    : Merlin_parser.t;
  recovering: ((int * Merlin_parser.t Location.loc) zipper) option;
}

let parser t = t.parser
let exns t = t.errors
let comments t = t.comments

let fresh parser = { errors = []; comments = [] ; parser; recovering = None }

let dump_recovering = function
  | None -> `Null
  | Some (Zipper (head, _, tail)) ->
    let dump_snapshot (priority,{Location. txt = parser; loc}) =
      let guide = loc.Location.loc_start in
      let line, col = Lexing.split_pos guide in
      `Assoc [
        "priority", `Int priority;
        "guide", `List [`Int line; `Int col];
        "parser", Merlin_parser.dump parser
      ]
    in
    `Assoc [
      "head", `List (List.map ~f:dump_snapshot head);
      "tail", `List (List.map ~f:dump_snapshot tail);
    ]

let dump t = `Assoc [
    "parser", Merlin_parser.dump t.parser;
    "recovery", dump_recovering t.recovering;
  ]

let dump_recoverable t =
  let t = match t.recovering with
    | Some _ -> t
    | None -> {t with recovering = Some (rollbacks Lexing.dummy_pos t.parser)}
  in
  dump t

let token_to_string tok =
  let open Merlin_parser.Values in
  string_of_class (class_of_symbol (symbol_of_token tok))

let dump_candidate (priority,{Location. txt = parser; loc}) =
  let guide = loc.Location.loc_start in
  let line, col = Lexing.split_pos guide in
  `Assoc [
    "priority", `Int priority;
    "guide", `List [`Int line; `Int col];
    "parser", Merlin_parser.dump parser
  ]

let rec feed_normal ~record_comment (s,tok,e as input) parser =
  let dump_token token = `Assoc [
      "token", `String (token_to_string token)
    ]
  in
  match Merlin_parser.feed ~record_comment input parser with
  | `Accept _ ->
    Logger.debugjf section ~title:"feed_normal accepted" dump_token tok;
    assert (tok = EOF);
    feed_normal ~record_comment (s,SEMISEMI,e) parser
  | (`Reject _ as result) ->
    Logger.debugjf section ~title:"feed_normal rejected" dump_token tok;
    result
  | (`Step parser as result) ->
    Logger.debugjf section ~title:"feed_normal step" dump_token tok;
    result

let closing_token = function
  | END -> true
  | RPAREN -> true
  | _ -> false

let prepare_candidates ref_col candidates =
  let open Location in
  let candidates = List.rev candidates in
  let cmp (pa,_ as ca) (pb,_ as cb) =
    match - compare pa pb with
      | 0 ->
        let la,ca = candidate_pos ca in
        let lb,cb = candidate_pos cb in
        begin match compare (abs (ca - ref_col) / 2) (abs (cb - ref_col) / 2) with
        | 0 -> - compare la lb
        | n -> n
        end
      | n -> n
  in
  List.stable_sort ~cmp candidates


let feed_recover ~record_comment original (s,tok,e as input) zipper =
  let _, ref_col = Lexing.split_pos s in
  let get_col candidate = snd (candidate_pos candidate) in
  (* Find appropriate recovering position *)
  let less_indented c = get_col c <= ref_col + 1 in
  let more_indented c = get_col c >= ref_col - 1 in
  (* Backward: increase column *)
  (* Forward: decrease column *)
  let zipper = Zipper.seek_forward more_indented zipper in
  let zipper = Zipper.seek_backward less_indented zipper in
  let candidates = Zipper.select_forward more_indented zipper in
  (*let candidates = Zipper.select_backward less_indented zipper in*)
  let candidates = prepare_candidates ref_col candidates in
  Logger.infojf section ~title:"feed_recover candidates"
    (fun (pos,candidates) ->
      `Assoc [
        "position", Lexing.json_of_position pos;
        "candidates", `List (List.map ~f:dump_candidate candidates)
      ])
    (s,candidates);
  let rec aux_feed n = function
    | [] -> Either.L zipper
    | candidate :: candidates ->
      aux_dispatch candidates n candidate
        (Merlin_parser.feed ~record_comment input (snd candidate).Location.txt)

  and aux_dispatch candidates n candidate = function
    | `Step parser ->
      Logger.infojf section ~title:"feed_recover selected"
        (fun (n,parser) ->
          `Assoc ["number", `Int n;
                  "parser", Merlin_parser.dump parser])
        (n,parser);
      Either.R parser
    | `Accept _ ->
      Logger.debugjf section ~title:"feed_recover accepted"
        (fun n -> `Assoc ["number", `Int n]) n;
      assert (tok = EOF);
      aux_dispatch candidates n candidate
        (Merlin_parser.feed ~record_comment (s,SEMISEMI,e)
           (snd candidate).Location.txt)
    | `Reject _ ->
      Logger.debugjf section ~title:"feed_recover rejected"
        (fun n -> `Assoc ["number", `Int n]) n;
      aux_feed (n + 1) candidates

  in
  aux_feed 0 candidates

let drop_comments_after pos comments =
  List.drop_while comments ~f:(fun (_, loc) ->
    Lexing.compare_pos loc.Location.loc_end pos > 0
  )

let fold warnings token t =
  match token with
  | Merlin_lexer.Error _ -> t
  | Merlin_lexer.Valid (s,tok,e) ->
    let s,e = match tok with
      | EOF -> let pos = {e with Lexing.
                              pos_lnum = e.Lexing.pos_lnum + 1;
                              pos_cnum = e.Lexing.pos_bol} in
        pos, pos
      | _ -> s, e in
    warnings := [];
    let pop w = let r = !warnings in w := []; r in
    let first_comments = drop_comments_after s t.comments in
    let recorded_comments = ref first_comments in
    let record_comment c = recorded_comments := c :: !recorded_comments in
    let recover_from t recovery =
      recorded_comments := first_comments ;
      match feed_recover ~record_comment t.parser (s,tok,e) recovery with
      | Either.L recovery ->
        {t with recovering = Some recovery ; comments = !recorded_comments}
      | Either.R parser ->
        {t with parser; recovering = None ; comments = !recorded_comments}
    in
    match t.recovering with
    | Some recovery -> recover_from t recovery
    | None ->
      begin match feed_normal ~record_comment (s,tok,e) t.parser with
        | `Reject invalid_parser ->
          let recovery = rollbacks e t.parser in
          Logger.infojf section ~title:"entering recovery"
            dump_recovering (Some recovery);
          let error = Error_classifier.from invalid_parser (s,tok,e) in
          recover_from
            {t with errors = error :: (pop warnings) @ t.errors}
            recovery
        | `Step parser ->
          let comments = !recorded_comments in
          {t with errors = (pop warnings) @ t.errors; parser; comments}
      end

let fold ?record_comment token t =
  let warnings = ref [] in
  Parsing_aux.catch_warnings warnings
    (fun () -> fold warnings token t)

