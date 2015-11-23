(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
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

module I = Parser_raw.MenhirInterpreter
module R = Parser_recover

type kind =
  | ML
  | MLI
  (*| MLL | MLY*)


type tree = [
  | `Signature of Parsetree.signature
  | `Structure of Parsetree.structure
]

type t = {
  kind: kind;
  tree: tree;
  errors: exn list;
}

let default = function
  | ML  -> `Structure []
  | MLI -> `Signature []

let eof_token = (Parser_raw.EOF, Lexing.dummy_pos, Lexing.dummy_pos)

let feed_token token env =
  let module T = struct
    open I
    type 'a checkpoint =
      | InputNeeded of env
      | Shifting of env * env * bool
      | AboutToReduce of env * production
      | HandlingError of env
      | Accepted of 'a
      | Rejected
    external inj : 'a checkpoint -> 'a I.checkpoint = "%identity"
  end in
  let rec aux = function
    | I.HandlingError _ | I.Rejected -> `Fail
    | I.Accepted v -> `Accept v
    | I.Shifting _ | I.AboutToReduce _ as checkpoint ->
      aux (I.resume checkpoint)
    | I.InputNeeded env as checkpoint -> `Recovered (checkpoint, env)
  in
  aux (I.offer (T.inj (T.InputNeeded env)) token)

let order_recoveries envs =
  let column env =
    match I.stack env with
    | None -> 0
    | Some stack ->
      let I.Element (_, _, startp, _) = I.stack_element stack in
      snd (Lexing.split_pos startp)
  in
  let cmp (c0, _) (c1, _) = compare (c1 : int) (c0 : int) in
  let envs = List.map ~f:(fun env -> column env, env) envs in
  List.stable_sort ~cmp envs

let attempt_recovery recoveries token =
  let _, startp, _ = token in
  let _, col = Lexing.split_pos startp in
  let more_indented (col', _) = col' > col + 1 in
  let recoveries = List.drop_while ~f:more_indented recoveries in
  let col = match recoveries with
    | (col', _) :: _ when col' < col -> col'
    | _ -> col
  in
  let same_indented (col', _) = col' >= col - 1 in
  let recoveries = List.take_while ~f:same_indented recoveries in
  let rec aux = function
    | [] -> `Fail
    | (_, x) :: xs -> match feed_token token x with
      | `Fail -> aux xs
      | `Recovered (checkpoint, _) -> `Ok checkpoint
      | `Accept v ->
        begin match aux xs with
          | `Fail -> `Accept v
          | x -> x
        end
  in
  aux recoveries

let rec recoveries env =
  match I.stack env with
  | None -> []
  | Some stack ->
    let I.Element (state, v, startp, endp) = I.stack_element stack in
    let decision =
      match R.decision (I.number state) with
      | R.Parent decide ->
        begin match I.stack_next stack with
          | None -> R.Pop
          | Some stack' ->
            let I.Element (state', _, _, _) =
              I.stack_element stack' in
            decide (I.number state')
        end
      | decision -> decision
    in
    let env =
      match decision with
      | R.Parent _ -> assert false
      | R.Pop -> I.pop env
      | R.Reduce prod ->
        Some (I.force_reduction (I.find_production prod) env)
      | R.Shift (I.N sym, v) ->
        Some (I.feed_nonterminal sym endp v endp env)
      | R.Shift (I.T sym, v) ->
        let token = Parser_printer.token_of_terminal sym v in
        match feed_token (token, endp, endp) env with
        | `Fail -> assert false
        | `Accept _ -> None
        | `Recovered (_,env) -> Some env
    in
    match env with
    | None -> []
    | Some env -> env :: recoveries env


let rec normal tokens checkpoint =
  match checkpoint with
  | I.InputNeeded env ->
    let token, tokens = match tokens with
      | token :: tokens -> token, tokens
      | [] -> eof_token, []
    in
    check_for_error token tokens env (I.offer checkpoint token)

  | I.Shifting _ | I.AboutToReduce _ ->
    normal tokens (I.resume checkpoint)

  | I.Accepted v -> v

  | I.Rejected | I.HandlingError _ ->
      assert false

and check_for_error token tokens env = function
  | I.HandlingError _ ->
    recover (order_recoveries (env :: recoveries env)) (token :: tokens)

  | I.Shifting _ | I.AboutToReduce _ as checkpoint ->
    check_for_error token tokens env (I.resume checkpoint)

  | checkpoint ->
    normal tokens checkpoint

and recover recoveries tokens =
  let token, tokens = match tokens with
    | token :: tokens -> token, tokens
    | [] -> eof_token, []
  in
  match attempt_recovery recoveries token with
  | `Fail -> recover recoveries tokens
  | `Accept v -> v
  | `Ok checkpoint ->
    normal tokens checkpoint

let run_parser tokens = function
  | ML  -> `Structure (normal tokens (Parser_raw.Incremental.implementation ()))
  | MLI -> `Signature (normal tokens (Parser_raw.Incremental.interface ()))

let run_parser lexer kind =
  try (run_parser (Merlin_lexer.tokens lexer) kind), []
  with exn -> (default kind), [exn]

let make lexer kind =
  let tree, errors = run_parser lexer kind in
  {kind; tree; errors}

let update lexer t =
  let tree, errors = run_parser lexer t.kind in
  {t with tree; errors}

let result t = t.tree

let errors t = t.errors
