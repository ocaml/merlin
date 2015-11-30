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
  lexer: Merlin_lexer.t;
}

let default = function
  | ML  -> `Structure []
  | MLI -> `Signature []

let eof_token = (Parser_raw.EOF, Lexing.dummy_pos, Lexing.dummy_pos)

module Recover
    (Parser : MenhirLib.IncrementalEngine.EVERYTHING)
    (Recovery : sig
       val default_value : 'a Parser.symbol -> 'a

       type action =
         | Shift  : 'a Parser.symbol -> action
         | Reduce : int -> action
         | Sub    : action list -> action
         | Pop    : action

       type decision =
         | Action of int * action
         | Parent of (int -> int * action)

       val decision : int -> decision

       val guide : 'a Parser.symbol -> bool

       val token_of_terminal : 'a Parser.terminal -> 'a -> Parser.token
     end)
    (Dump : sig
       val token   : Parser.token -> string
       val element : cursor -> Parser.element -> unit
       val item    : cursor -> Parser.item -> unit
       val env     : cursor -> _ Parser.env -> unit
     end) : sig

  type 'a candidate = {
    line: int;
    min_col: int;
    max_col: int;
    env: 'a Parser.env;
  }

  type 'a candidates = {
    final: 'a option;
    candidates: 'a candidate list;
  }

  val attempt :
    cursor -> 'a candidates ->
    Parser.token * Lexing.position * Lexing.position ->
    [> `Accept of 'a
    | `Fail
    | `Ok of 'a Parser.checkpoint * 'a Parser.env ]

  val generate :
    cursor -> 'a Parser.env -> 'a candidates

  val dump :
    Nav.t ->
    wrong:(Parser.token * Lexing.position * Lexing.position) ->
    rest:(Parser.token * Lexing.position * Lexing.position) list ->
    'a Parser.env -> unit

end = struct

  type 'a candidate = {
    line: int;
    min_col: int;
    max_col: int;
    env: 'a Parser.env;
  }

  type 'a candidates = {
    final: 'a option;
    candidates: 'a candidate list;
  }

  module T = struct
    type 'a checkpoint =
      | InputNeeded of 'a Parser.env
      | Shifting of 'a Parser.env * 'a Parser.env * bool
      | AboutToReduce of 'a Parser.env * Parser.production
      | HandlingError of 'a Parser.env
      | Accepted of 'a
      | Rejected
    external inj : 'a checkpoint -> 'a Parser.checkpoint = "%identity"
  end

  let env_state env =
    match Parser.stack env with
    | None -> -1
    | Some stack ->
      let Parser.Element (state, _, _, _) = Parser.stack_element stack in
      Parser.number state

  let feed_token ~allow_reduction token env =
    let rec aux allow_reduction = function
      | Parser.HandlingError _ | Parser.Rejected -> `Fail
      | Parser.AboutToReduce _ when not allow_reduction -> `Fail
      | Parser.Accepted v -> `Accept v
      | Parser.Shifting _ | Parser.AboutToReduce _ as checkpoint ->
        aux true (Parser.resume checkpoint)
      | Parser.InputNeeded env as checkpoint -> `Recovered (checkpoint, env)
    in
    aux allow_reduction (Parser.offer (T.inj (T.InputNeeded env)) token)

  let rec follow_guide col = function
    | None -> col
    | Some stack ->
      let Parser.Element (state, _, pos, _) =
        Parser.stack_element stack in
      if Recovery.guide (Parser.incoming_symbol state) then
        follow_guide
          (snd (Lexing.split_pos pos)) (Parser.stack_next stack)
      else
        col

  let candidate depth env =
    let line, min_col, max_col =
      match Parser.stack env with
      | None -> 1, 0, 0
      | Some stack ->
        let Parser.Element (_, _, pos, _) = Parser.stack_element stack in
        let line, col = Lexing.split_pos pos in
        if depth = 0 then
          line, col, col
        else
          let rec aux depth = function
            | None -> max_int
            | Some stack when depth = 0 ->
              let Parser.Element (_, _, pos, _) = Parser.stack_element stack in
              follow_guide
                (snd (Lexing.split_pos pos)) (Parser.stack_next stack)
            | Some stack ->
              aux (depth - 1) (Parser.stack_next stack)
          in
          let col' = aux (depth - 1) (Parser.stack_next stack) in
          line, min col col', max col col'
    in
    { line; min_col; max_col; env }

  let attempt k r token =
    let _, startp, _ = token in
    let line, col = Lexing.split_pos startp in
    let more_indented candidate =
      line <> candidate.line && candidate.min_col > col in
    let recoveries = List.drop_while ~f:more_indented r.candidates in
    let same_indented candidate =
      line = candidate.line ||
      (candidate.min_col <= col && col <= candidate.max_col)
    in
    let recoveries = List.take_while ~f:same_indented recoveries in
    let rec aux = function
      | [] -> `Fail
      | x :: xs -> match feed_token ~allow_reduction:false token x.env with
        | `Fail ->
          if not (is_closed k) then
            printf k "Couldn't resume %d with %S.\n"
              (env_state x.env) (let (t,_,_) = token in Dump.token t);
          aux xs
        | `Recovered (checkpoint, _) -> `Ok (checkpoint, x.env)
        | `Accept v ->
          begin match aux xs with
            | `Fail -> `Accept v
            | x -> x
          end
    in
    aux recoveries

  let generate k (type a) (env : a Parser.env) =
    let module E = struct
      exception Result of a
    end in
    let rec aux acc env =
      match Parser.stack env with
      | None -> None, acc
      | Some stack ->
        let elt = Parser.stack_element stack in
        let Parser.Element (state, v, startp, endp) = elt in
        Dump.element k elt;
        let depth, action =
          match Recovery.decision (Parser.number state) with
          | Recovery.Parent select_action ->
            begin match Parser.stack_next stack with
              | None -> 0, Recovery.Pop
              | Some stack' ->
                let Parser.Element (state', _, _, _) =
                  Parser.stack_element stack' in
                select_action (Parser.number state')
            end
          | Recovery.Action (depth, action) -> depth, action
        in
        let candidate0 = candidate depth env in
        let rec eval (env : a Parser.env) : Recovery.action -> a Parser.env = function
          | Recovery.Pop ->
            (match Parser.pop env with
             | None -> raise Not_found
             | Some env -> env)
          | Recovery.Reduce prod ->
            let prod = Parser.find_production prod in
            begin try
                Parser.force_reduction prod env
              with exn ->
                printf k "Error %S in force_reduction, reducing:\n"
                  (Printexc.to_string exn);
                Dump.item k (prod, -1);
                printf k "In environment:\n";
                Dump.env k env;
                raise exn
            end
          | Recovery.Shift (Parser.N n as sym) ->
            let v = Recovery.default_value sym in
            Parser.feed_nonterminal n endp v endp env
          | Recovery.Shift (Parser.T t as sym) ->
            let v = Recovery.default_value sym in
            let token = (Recovery.token_of_terminal t v, endp, endp) in
            begin match feed_token ~allow_reduction:true token env with
              | `Fail -> assert false
              | `Accept v -> raise (E.Result v)
              | `Recovered (_,env) -> env
            end
          | Recovery.Sub actions ->
            List.fold_left ~f:eval ~init:env actions
        in
        match begin
          let envs = match action with
            | Recovery.Sub actions -> List.rev_scan_left [] ~f:eval ~init:env actions
            | action -> [eval env action]
          in
          List.map ~f:(fun env -> {candidate0 with env}) envs
        end with
        | exception Not_found -> None, acc
        | exception (E.Result v) -> Some v, acc
        | [] -> None, acc
        | (candidate :: _) as candidates ->
          aux (candidates @ acc) candidate.env
    in
    aux [] env

  let generate k env =
    let final, candidates = generate k env in
    let candidates =
      List.rev_filter ~f:(fun { env } ->
          match Parser.stack env with
          | None -> false
          | Some stack ->
            let Parser.Element (state, _, _, _) =
              Parser.stack_element stack in
            Parser.default_reduction state = None)
        candidates
    in
    { final; candidates = (candidate 0 env) :: candidates }

  let dump nav ~wrong:(t,s,e as token) ~rest:tokens env =
    let body = Nav.body nav in
    if not (is_closed body) then (
      let l, c = Lexing.split_pos s in
      printf body "Unexpected %S at %d:%d, " (Dump.token t) l c;
      link body "see recoveries"
        (fun _ -> Nav.modal nav "Recoveries" @@ fun nav ->
          let body = Nav.body nav in
          let r = generate body env in
          let rec aux = function
            | [] -> ()
            | token :: tokens ->
              match attempt body r token with
              | `Fail -> aux tokens
              | `Accept v ->
                text body "\nCouldn't resume, generated final AST.\n"
              | `Ok (checkpoint, recovered_from) ->
                printf body "\nResumed with %S from:\n"
                  (let (t,_,_) = token in Dump.token t);
                Dump.env body recovered_from
          in
          aux (token :: tokens)
        );
      text body ".\n";
      Dump.env body env;
      text body "\n"
    )
end

module R = Recover (I)
    (struct
      include Parser_recover

      let guide (type a) : a I.symbol -> bool = function
        | I.T I.T_BEGIN -> true
        | _ -> false

      let token_of_terminal = Parser_printer.token_of_terminal
    end)
    (struct
      let token = Parser_printer.print_token

      let position pos =
        let l1, c1 = Lexing.split_pos pos in
        Printf.sprintf "%d:%d" l1 c1

      let item k (prod, pos) =
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
          List.iter (item k) items
        )

      let element k (I.Element (state, _, startp, endp)) =
        if not (is_closed k) then (
          printf k "From %s to %s, " (position startp) (position endp);
          print_state k state
        )

      let env k env =
        match I.stack env with
        | None ->
          text k "Initial state."
        | Some stack ->
          element k (I.stack_element stack)
    end)

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
      R.dump nav ~wrong:token ~rest:tokens env;
      recover (R.generate null_cursor env) (token :: tokens)

    | I.Shifting _ | I.AboutToReduce _ as checkpoint ->
      check_for_error token tokens env (I.resume checkpoint)

    | checkpoint ->
      normal tokens checkpoint

  and recover recoveries tokens =
    let token, tokens = match tokens with
      | token :: tokens -> token, tokens
      | [] -> eof_token, []
    in
    match R.attempt null_cursor recoveries token with
    | `Fail ->
      if tokens = [] then
        match recoveries.R.final with
        | None -> failwith "Empty file"
        | Some v -> v
      else
        recover recoveries tokens
    | `Accept v -> v
    | `Ok (checkpoint, _) ->
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
  {kind; tree; errors; lexer}

let update lexer t =
  if t.lexer == lexer then
    t
  else if Merlin_lexer.compare lexer t.lexer = 0 then
    {t with lexer}
  else
    let tree, errors = run_parser Nav.null lexer t.kind in
    {t with tree; errors; lexer}

let trace nav lexer t =
  ignore (run_parser nav lexer t.kind)

let result t = t.tree

let errors t = t.errors

let lexer t = t.lexer

let compare t1 t2 =
  if t1.kind = t2.kind then
    Merlin_lexer.compare t1.lexer t2.lexer
  else
    compare t1 t2
