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

  let env_state env =
    match I.stack env with
    | None -> -1
    | Some stack ->
      let I.Element (state, _, _, _) = I.stack_element stack in
      I.number state

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

let guide_symbol (type a) : a I.symbol -> bool = function
  | I.T I.T_BEGIN -> true
  | _ -> false

let get_pos depth env =
  match I.stack env with
  | None -> 1, 0, 0
  | Some stack ->
    let I.Element (_, _, startp, _) = I.stack_element stack in
    let line, col = Lexing.split_pos startp in
    if depth = 0 then
      line, col, col
    else
      let rec follow_guide col = function
        | None -> col
        | Some stack ->
          let I.Element (state, _, startp, _) = I.stack_element stack in
          if guide_symbol (I.incoming_symbol state) then
            follow_guide (snd (Lexing.split_pos startp)) (I.stack_next stack)
          else
            col
      in
      let rec aux depth = function
        | None -> max_int
        | Some stack when depth = 0 ->
          let I.Element (_, _, startp, _) = I.stack_element stack in
          follow_guide (snd (Lexing.split_pos startp)) (I.stack_next stack)
        | Some stack ->
          aux (depth - 1) (I.stack_next stack)
      in
      let col' = aux (depth - 1) (I.stack_next stack) in
      line, min col col', max col col'

let attempt_recovery k recoveries token =
  let _, startp, _ = token in
  let line, col = Lexing.split_pos startp in
  let more_indented ((line', col0, col1), _) =
    line <> line' && col0 > col in
  let recoveries = List.drop_while ~f:more_indented recoveries in
  let same_indented ((line', col0, col1), _) =
    line = line' || (col0 <= col && col <= col1) in
  let recoveries = List.take_while ~f:same_indented recoveries in
  let rec aux = function
    | [] -> `Fail
    | (_, x) :: xs -> match feed_token ~allow_reduction:false token x with
      | `Fail ->
        if not (is_closed k) then
          printf k "Couldn't resume %d with %S.\n"
            (Printing.env_state x)
            (Parser_printer.print_token @@
             let (t,_,_) = token in t);
        aux xs
      | `Recovered (checkpoint, env) -> `Ok (checkpoint, env, x)
      | `Accept v ->
        begin match aux xs with
          | `Fail -> `Accept v
          | x -> x
        end
  in
  aux recoveries

let generate_recoveries k env =
  let module E = struct
    exception Result of Obj.t
  end in
  let rec aux acc env =
    match I.stack env with
    | None -> None, acc
    | Some stack ->
      let I.Element (state, v, startp, endp) as elt = I.stack_element stack in
      Printing.print_element k elt;
      let depth, action =
        match R.decision (I.number state) with
        | R.Parent select_action ->
          begin match I.stack_next stack with
            | None -> 0, R.Pop
            | Some stack' ->
              let I.Element (state', _, _, _) =
                I.stack_element stack' in
              select_action (I.number state')
          end
        | R.Action (depth, action) -> depth, action
      in
      let pos = get_pos depth env in
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
           | `Accept v -> raise (E.Result v)
           | `Recovered (_,env) -> env)
        | R.Sub actions ->
          List.fold_left ~f:eval ~init:env actions
      in
      match begin
        let envs = match action with
          | R.Sub actions -> List.rev_scan_left [] ~f:eval ~init:env actions
          | action -> [eval env action]
        in
        List.map ~f:(fun env -> pos, env) envs
      end with
      | exception Not_found -> None, acc
      | exception (E.Result v) -> Some v, acc
      | [] -> None, acc
      | ((_, env) :: _) as envs -> aux (envs @ acc) env
  in
  aux [] env

let recoveries k env =
  let final, recoveries = generate_recoveries k env in
  let recoveries =
    List.rev_filter ~f:(fun (_,env) ->
        match I.stack env with
        | None -> false
        | Some stack ->
          let I.Element (state, _, _, _) = I.stack_element stack in
          I.default_reduction state = None)
      recoveries
  in
  final, (get_pos 0 env, env) :: recoveries

let show_recoveries nav (t,s,e as token) tokens env =
  let body = Nav.body nav in
  if not (is_closed body) then (
    printf body "Unexpected %S at %s, "
      (Parser_printer.print_token t)
      (Printing.print_position s);
    link body "see recoveries"
      (fun _ -> Nav.modal nav "Recoveries" @@ fun nav ->
        let body = Nav.body nav in
        let _final, r = recoveries body env in
        let rec aux = function
          | [] -> ()
          | token :: tokens ->
            match attempt_recovery body r token with
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
      recover (recoveries null_cursor env) (token :: tokens)

    | I.Shifting _ | I.AboutToReduce _ as checkpoint ->
      check_for_error token tokens env (I.resume checkpoint)

    | checkpoint ->
      normal tokens checkpoint

  and recover recoveries tokens =
    let token, tokens = match tokens with
      | token :: tokens -> token, tokens
      | [] -> eof_token, []
    in
    match attempt_recovery null_cursor (snd recoveries) token with
    | `Fail ->
      if tokens = [] then
        match fst recoveries with
        | None -> failwith "Empty file"
        | Some v -> Obj.magic v
      else
        recover recoveries tokens
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
