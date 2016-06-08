open Std
open Sturgeon_stub
open Cursor
open Widget
type cursor = Sturgeon_stub.cursor

module Make
    (Parser : MenhirLib.IncrementalEngine.EVERYTHING)
    (Recovery : sig
       val default_value : Location.t -> 'a Parser.symbol -> 'a

       type action =
         | Abort
         | R of int
         | S : 'a Parser.symbol -> action
         | Sub of action list

       type decision =
         | Nothing
         | One of action list
         | Select of (int -> action list)

       val depth : int array

       val can_pop : 'a Parser.terminal -> bool

       val recover : int -> decision

       val guide : 'a Parser.symbol -> bool

       val token_of_terminal : 'a Parser.terminal -> 'a -> Parser.token

       val nullable : 'a Parser.nonterminal -> bool
     end)
    (Dump : sig
       val token   : Parser.token -> string
       val symbol  : Parser.xsymbol -> string
       val element : cursor -> Parser.element -> unit
       val item    : cursor -> Parser.item -> unit
       val env     : cursor -> _ Parser.env -> unit
     end) =
struct

  type 'a candidate = {
    line: int;
    min_col: int;
    max_col: int;
    env: 'a Parser.env;
  }

  type 'a candidates = {
    popped: Parser.xsymbol list;
    shifted: Parser.xsymbol option;
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

  let candidate env =
    let line, min_col, max_col =
      match Parser.stack env with
      | None -> 1, 0, 0
      | Some stack ->
        let Parser.Element (state, _, pos, _) = Parser.stack_element stack in
        let depth = Recovery.depth.(Parser.number state) in
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
    let print_candidates xs =
      printf k "{ ";
      let p x =
        printf k "%d@%d:%d-%d" (env_state x.env) x.line x.min_col x.max_col in
      begin match xs with
      | [] -> ()
      | x :: xs ->
          p x; List.iter (fun x -> printf k ", "; p x) xs;
      end;
      printf k "}"
    in
    printf k "Recovery from %s @ %d:%d\n"
      (let (t,_,_) = token in Dump.token t) line col;
    printf k "Candidates = ";
    print_candidates recoveries;
    printf k "\n";
    let recoveries = List.take_while ~f:same_indented recoveries in
    printf k "Selected = ";
    print_candidates recoveries;
    printf k "\n";
    let rec aux = function
      | [] -> `Fail
      | x :: xs -> match feed_token ~allow_reduction:true token x.env with
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

  let decide stack =
    let rec nth_state stack n =
      if n = 0 then
        let Parser.Element (state, _, _, _) = Parser.stack_element stack in
        Parser.number state
      else
        match Parser.stack_next stack with
        | None -> assert (n = 1); -1
        | Some stack -> nth_state stack (n - 1)
    in
    let st = nth_state stack 0 in
    match Recovery.recover st with
    | Recovery.Nothing -> []
    | Recovery.One actions -> actions
    | Recovery.Select f -> f (nth_state stack Recovery.depth.(st))

  let generate k (type a) (env : a Parser.env) =
    let module E = struct
      exception Result of a
    end in
    let shifted = ref None in
    let rec aux acc env =
      match Parser.stack env with
      | None -> None, acc
      | Some stack ->
        let elt = Parser.stack_element stack in
        let Parser.Element (state, v, startp, endp) = elt in
        Dump.element k elt;
        Logger.log "recover" "decide state" (string_of_int (Parser.number state));
        let actions = decide stack in
        let candidate0 = candidate env in
        let rec eval (env : a Parser.env) : Recovery.action -> a Parser.env = function
          | Recovery.Abort ->
            Logger.log "recover" "eval Abort" "";
            raise Not_found
          | Recovery.R prod ->
            Logger.log "recover" "eval Reduce" "";
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
          | Recovery.S (Parser.N n as sym) ->
            let xsym = Parser.X sym in
            if !shifted = None && not (Recovery.nullable n) then
              shifted := Some xsym;
            Logger.log "recover" "eval Shift N" (Dump.symbol xsym);
            let loc = {Location. loc_start = endp; loc_end = endp; loc_ghost = true} in
            let v = Recovery.default_value loc sym in
            Parser.feed_nonterminal n endp v endp env
          | Recovery.S (Parser.T t as sym) ->
            let xsym = Parser.X sym in
            if !shifted = None then shifted := Some xsym;
            Logger.log "recover" "eval Shift T" (Dump.symbol xsym);
            let loc = {Location. loc_start = endp; loc_end = endp; loc_ghost = true} in
            let v = Recovery.default_value loc sym in
            let token = (Recovery.token_of_terminal t v, endp, endp) in
            begin match feed_token ~allow_reduction:true token env with
              | `Fail -> assert false
              | `Accept v -> raise (E.Result v)
              | `Recovered (_,env) -> env
            end
          | Recovery.Sub actions ->
            Logger.log "recover" "enter Sub" "";
            let env = List.fold_left ~f:eval ~init:env actions in
            Logger.log "recover" "leave Sub" "";
            env
        in
        match
          List.rev_scan_left [] ~f:eval ~init:env actions
          |> List.map ~f:(fun env -> {candidate0 with env})
        with
        | exception Not_found -> None, acc
        | exception (E.Result v) -> Some v, acc
        | [] -> None, acc
        | (candidate :: _) as candidates ->
          aux (candidates @ acc) candidate.env
    in
    let popped = ref [] in
    (*let should_pop stack =
      let Parser.Element (state, _, _, _) = Parser.stack_element stack in
      match Parser.incoming_symbol state with
      | (Parser.T term) as t1 when Recovery.can_pop term ->
        Logger.logf "recover" "Pop" "pop %s"
          (Dump.symbol (Parser.X t1));
        begin match Parser.stack_next stack with
          | None -> false
          | Some stack' ->
            let rec check_next = function
              | Recovery.S (Parser.T term' as t2) :: _
                when Parser.X t1 = Parser.X t2 ->
                false
              | Recovery.S sym :: _ ->
                Logger.logf "recover" "Pop" "then push %s"
                  (Dump.symbol (Parser.X sym));
                popped := Parser.X t1 :: !popped;
                true
              | Recovery.Sub xs :: _ ->
                check_next xs
              | _ ->
                popped := Parser.X t1 :: !popped;
                true
            in
            check_next (decide stack')
        end
      | _ -> false
      in*)
    let should_pop _ = false in
    let rec pop_first env =
      match Parser.stack env with
      | Some stack when should_pop stack ->
        begin match Parser.pop env with
          | None -> assert false
          | Some env' ->
            Logger.log "recover" "pre-eval Pop" "";
            pop_first env'
        end
      | _ -> env
    in
    let final, candidates = aux [] (pop_first env) in
    (List.rev !popped, !shifted, final, candidates)

  let generate k env =
    let popped, shifted, final, candidates = generate k env in
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
    { popped; shifted; final; candidates = (candidate env) :: candidates }

  let dump {Nav. nav; body} ~wrong:(t,s,e as token) ~rest:tokens env =
    if not (is_closed body) then (
      let l, c = Lexing.split_pos s in
      printf body "Unexpected %S at %d:%d, " (Dump.token t) l c;
      link body "see recoveries"
        (fun _ -> Nav.push nav "Recoveries" @@ fun {Nav. body} ->
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
