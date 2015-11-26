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
open Sturgeon.Tui

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

module Printing = struct

  let print_position pos =
    let l1, c1 = Lexing.split_pos pos in
    Printf.sprintf "%d:%d" l1 c1


  let print_item k (prod, pos) =
    if not (is_closed k) then (
      let lhs = Parser_printer.print_symbol (I.lhs prod) in
      let rec insert_dot pos = function
        | [] -> ["."]
        | xs when pos = 0 -> "." :: xs
        | x :: xs -> x :: insert_dot (pos - 1) xs
      in
      let rhs =
        I.rhs prod
        |> List.map ~f:Parser_printer.print_symbol
        |> insert_dot pos
        |> String.concat ~sep:" "
      in
      printf k "%s ::= %s\n" lhs rhs
    )

  let print_state k state =
    if not (is_closed k) then (
      let items = I.items state in
      printf k "LR(1) state %d:\n" (I.number state);
      List.iter (print_item k) items
    )

  let print_element k (I.Element (state, _, startp, endp)) =
    if not (is_closed k) then (
      printf k "From %s to %s, "
        (print_position startp)
        (print_position endp);
      print_state k state
    )

  let print_env_summary k env =
    match I.stack env with
    | None ->
      text k "Initial state."
    | Some stack ->
      print_element k (I.stack_element stack)

end


let feed_token ?(allow_reduction=true) token env =
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
  let rec aux allow_reduction = function
    | I.HandlingError _ | I.Rejected -> `Fail
    | I.AboutToReduce _ when not allow_reduction -> `Fail
    | I.Accepted v -> `Accept v
    | I.Shifting _ | I.AboutToReduce _ as checkpoint ->
      aux true (I.resume checkpoint)
    | I.InputNeeded env as checkpoint -> `Recovered (checkpoint, env)
  in
  aux allow_reduction (I.offer (T.inj (T.InputNeeded env)) token)

let order_recoveries envs =
  let pos env =
    match I.stack env with
    | None -> 1, 0
    | Some stack ->
      let I.Element (_, _, startp, _) = I.stack_element stack in
      Lexing.split_pos startp
  in
  List.map ~f:(fun env -> pos env, env) envs
  (*let cmp (c0, _) (c1, _) = compare (c1 : int) (c0 : int) in
  let envs = List.map ~f:(fun env -> column env, env) envs in
  List.stable_sort ~cmp envs*)

let attempt_recovery recoveries token =
  let _, startp, _ = token in
  let line, col = Lexing.split_pos startp in
  let more_indented ((line', col'), _) = line = line' || col' > col in
  let recoveries = List.drop_while ~f:more_indented recoveries in
  let same_indented ((line', col'), _) = line = line' || abs (col' - col) <= 1 in
  let recoveries = List.take_while ~f:same_indented recoveries in
  let rec aux = function
    | [] -> `Fail
    | (_, x) :: xs -> match feed_token ~allow_reduction:false token x with
      | `Fail -> aux xs
      | `Recovered (checkpoint, env) -> `Ok (checkpoint, env, x)
      | `Accept v ->
        begin match aux xs with
          | `Fail -> `Accept v
          | x -> x
        end
  in
  aux recoveries

let rec recoveries k acc env =
  match I.stack env with
  | None -> acc
  | Some stack ->
    let I.Element (state, v, startp, endp) as elt = I.stack_element stack in
    Printing.print_element k elt;
    let action =
      match R.decision (I.number state) with
      | R.Parent select_action ->
        begin match I.stack_next stack with
          | None -> R.Pop
          | Some stack' ->
            let I.Element (state', _, _, _) =
              I.stack_element stack' in
            select_action (I.number state')
        end
      | R.Action action -> action
    in
    let rec eval env = function
      | R.Pop ->
        (match I.pop env with
         | None -> raise Not_found
         | Some env -> env)
      | R.Reduce prod ->
        begin try
            I.force_reduction (I.find_production prod) env
          with exn ->
            printf k "Error %S in force_reduction, reducing:\n"
              (Printexc.to_string exn);
            Printing.print_item k (Obj.magic prod, -1);
            printf k "In environment:\n";
            Printing.print_env_summary k env;
            raise exn
        end
      | R.Shift (I.N n as sym) ->
        let v = Parser_recover.default_value sym in
        I.feed_nonterminal n endp v endp env
      | R.Shift (I.T t as sym) ->
        let v = Parser_recover.default_value sym in
        let token = Parser_printer.token_of_terminal t v in
        (match feed_token (token, endp, endp) env with
         | `Fail -> assert false
         | `Accept _ -> raise Not_found
         | `Recovered (_,env) -> env)
      | R.Sub actions ->
        List.fold_left ~f:eval ~init:env actions
    in
    let top_eval env = function
      | R.Sub actions -> List.rev_scan_left [] ~f:eval ~init:env actions
      | action -> [eval env action]
    in
    match top_eval env action with
    | exception Not_found -> acc | [] -> acc
    | (env :: _) as envs -> recoveries k (envs @ acc) env

let recoveries k env =
  List.rev_filter ~f:(fun env ->
      match I.stack env with
      | None -> false
      | Some stack ->
        let I.Element (state, _, startp, endp) = I.stack_element stack in
        I.default_reduction state = None)
    (recoveries k [] env)

let show_recoveries nav (t,s,e as token) tokens env =
  let body = Nav.body nav in
  if not (is_closed body) then (
    printf body "Unexpected %S at %s, "
      (Parser_printer.print_token t)
      (Printing.print_position s);
    link body "see recoveries"
      (fun _ -> Nav.modal nav "Recoveries" @@ fun nav ->
        let body = Nav.body nav in
        let r = env :: recoveries body env in
        let r = order_recoveries r in
        let rec aux = function
          | [] -> ()
          | token :: tokens ->
            match attempt_recovery r token with
            | `Fail -> aux tokens
            | `Accept v ->
              text body "\nCouldn't resume, generated final AST.\n"
            | `Ok (checkpoint, _, recovered_from) ->
              printf body "\nResumed with %S from:\n"
                (Parser_printer.print_token @@
                 let (t,_,_) = token in t);
              Printing.print_env_summary body recovered_from
        in
        aux (token :: tokens)
      );
    text body ".\n";
    Printing.print_env_summary body env;
    text body "\n"
  )

let parse nav =
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
      show_recoveries nav token tokens env;
      recover (order_recoveries (env :: recoveries null_cursor env)) (token :: tokens)

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
    | `Ok (checkpoint, _, _) ->
      normal tokens checkpoint
  in
  normal

let run_parser k tokens = function
  | ML  -> `Structure (parse k tokens (Parser_raw.Incremental.implementation ()))
  | MLI -> `Signature (parse k tokens (Parser_raw.Incremental.interface ()))

let run_parser k lexer kind =
  try (run_parser k (Merlin_lexer.tokens lexer) kind), []
  with exn -> (default kind), [exn]

let make lexer kind =
  let tree, errors = run_parser Nav.null lexer kind in
  {kind; tree; errors}

let update lexer t =
  let tree, errors = run_parser Nav.null lexer t.kind in
  {t with tree; errors}

let trace nav lexer t =
  ignore (run_parser nav lexer t.kind)

let result t = t.tree

let errors t = t.errors
