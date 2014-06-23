open Std

module Values = Raw_parser_values

module P = Raw_parser
module E = MenhirLib.EngineTypes

type state = Raw_parser.state

type t = Parser of P.feed P.parser * MenhirUtils.witness
type frame = Frame of int * (P.state, P.symbol) E.stack

type parser = t

let get_stack s = s.P.env.E.stack
let get_state s = s.P.env.E.current
let mk_loc loc_start loc_end = {Location. loc_start; loc_end; loc_ghost = false}

module Frame : sig
  val stack : t -> frame
  val depth : frame -> int

  val value : frame -> P.symbol
  val location : frame -> Location.t
  val eq    : frame -> frame -> bool
  val next  : frame -> frame option

  (* Ease pattern matching on parser stack *)
  type destruct = D of P.symbol * destruct lazy_t
  val destruct: frame -> destruct
end = struct

  let frame_of d stack =
    if stack.E.next == stack then
      None
    else
      Some (Frame (d,stack))

  let stack (Parser (parser,w)) =
    match frame_of (MenhirUtils.depth w - 1) (get_stack parser) with
    | Some frame -> frame
    | None -> assert false

  let depth (Frame (d,f)) = d

  let value (Frame (_,frame)) = frame.E.semv

  let location (Frame (_,frame)) =
    mk_loc frame.E.startp frame.E.endp

  let eq (Frame (_,f)) (Frame (_,f')) = f == f'

  let next (Frame (d,f)) = frame_of (d - 1) f.E.next

  (* Ease pattern matching on parser stack *)
  type destruct = D of P.symbol * destruct lazy_t
  let rec destruct_bottom = D (P.Bottom, lazy destruct_bottom)

  let rec destruct f =
    let v = value f in
    let tl = match next f with
      | Some f' -> lazy (destruct f')
      | None -> lazy destruct_bottom
    in
    D (v,tl)

end

let implementation = P.implementation_state
let interface = P.interface_state

let stack = Frame.stack

let pop (Parser (p, depth)) =
  match MenhirUtils.pop p.P.env with
  | None -> None
  | Some env ->
    let p = {p with P.env = env} in
    Some (Parser (p, MenhirUtils.stack_depth ~hint:depth (get_stack p)))

let location t =
  Frame.location (stack t)

let of_feed p depth =
  Parser (p, MenhirUtils.stack_depth ~hint:depth (get_stack p))

let rec of_step s depth =
  match P.step s with
  | `Accept _ | `Reject as result -> result
  | `Feed p -> `Step (of_feed p depth)
  | `Step p -> of_step p (MenhirUtils.stack_depth ~hint:depth (get_stack p))

let to_step (Parser (step,_)) = step

let from state input =
  match of_step (P.initial state input) MenhirUtils.initial_depth with
  | `Step p -> p
  | _ -> assert false

let feed (s,t,e as input) parser =
  match t with
  (* Ignore comments *)
  | P.COMMENT _ -> `Step parser
  | _ ->
    let Parser (p, depth) = parser in
    let p' = P.feed p input in
    of_step p' depth

let dump_item ppf (prod, dot_pos) =
  let print_symbol i symbol =
    Format.fprintf ppf "%s %s"
      (if i = dot_pos then " ." else "")
      (Values.string_of_class symbol)
  in
  let lhs, rhs = P.Query.production_definition prod in
  Format.fprintf ppf "%s = "
    (Option.value_map ~f:Values.string_of_class ~default:"?" lhs);
  List.iteri print_symbol rhs

let dump_itemset ppf l =
  List.iter ~f:(Format.fprintf ppf "- %a\n" dump_item) l

let dump ppf t =
  (* Print current frame, with its itemset *)
  let Parser (s,_) = t in
  let state = get_state s in
  let itemset = P.Query.itemset state in
  Format.fprintf ppf "position: %a; itemset:\n%a"
    Lexing.print_position (location t).Location.loc_start
    dump_itemset itemset;
  (* Print overview of the stack *)
  let rec aux first ppf = function
    | None -> ()
    | Some frame ->
      let v = Frame.value frame in
      let l,c = Lexing.split_pos (Frame.location frame).Location.loc_start in
      Format.fprintf ppf "%s(%d.) %s %d:%d"
        (if first then "" else "; ")
        (Frame.depth frame)
        Values.(string_of_class (class_of_symbol v)) l c;
      aux false ppf (Frame.next frame)
  in
  Format.fprintf ppf "[";
  aux true ppf (Some (Frame.stack t));
  Format.fprintf ppf "]\n%!"

let last_token (Parser (raw_parser,_)) =
  let loc_start,t,loc_end = raw_parser.P.env.E.token in
  Location.mkloc t
    {Location. loc_start; loc_end; loc_ghost = false}

let find_strategies (Parser (p,w)) =
  let env = p.P.env in
  let lr1_state = env.E.current in
  let lr0_state = P.Query.lr0_state lr1_state in
  let strategies = Merlin_recovery_strategy.reduction_strategy lr0_state in
  Logger.errorf `parser (fun ppf strategies ->
    Format.fprintf ppf "search for strategies at %d.\n" lr0_state;
    Format.fprintf ppf "itemset:\n%a"
      dump_itemset (P.Query.itemset lr0_state);
    if strategies = [] then
      Format.fprintf ppf "no candidate selected, dropping.\n"
    else
      begin
        Format.fprintf ppf "candidates: (selected in first position)\n";
        List.iter strategies
          ~f:(fun {Merlin_recovery_strategy. cost; action} ->
              match action with
              | `Reduce {Merlin_recovery_strategy. r_prod; r_symbols = l} ->
                let l = List.map ~f:Values.class_of_symbol l in
                let l = List.map ~f:Values.string_of_class l in
                Format.fprintf ppf
                  "- at cost %d, reduce %d, %s\n" cost r_prod
                  (String.concat " " l)
              | `Shift (token, priority) ->
                let token = Values.symbol_of_token token in
                let token = Values.class_of_symbol token in
                let token = Values.string_of_class token in
                Format.fprintf ppf
                  "- at cost %d, priority %d, shift %s\n" cost priority token
          )
      end
  ) strategies;
  strategies

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
  | parser, termination, false -> recover ?endp termination parser
  | Parser (p,w), termination, true ->
    let env = p.P.env in
    let add_symbol stack symbol =
      let endp = match endp with
        | Some endp -> endp
        | None -> stack.E.endp
      in
      {stack with E. semv = symbol; startp = stack.E.endp; endp; next = stack}
    in
    (* Feed stack *)
    match strat.action with
    | `Reduce {r_prod; r_symbols; r_action} ->
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
      let parser = of_feed {p with P. env} w in
      Some (termination, parser_priority parser, parser)

    | `Shift (token,priority) ->
      let startp = (location parser).Location.loc_end in
      let endp = Option.value ~default:startp endp in
      match feed (startp,token,endp) parser with
      | `Accept _ | `Reject -> None
      | `Step parser' -> Some (termination, priority, parser')

module Integrate
    (P : sig
       (* Arbitrary state, passed to update functions *)
       type st
       type t
       val empty : st -> t (* Base-case, empty stack *)
       val frame : st -> frame -> t -> t (* Add frame *)

       (* Default: delta st f t ~old:_ = frame st f t *)
       val delta : st -> frame -> t -> old:(t * frame) -> t
       (* Check if an intermediate result is still valid *)
       val validate : st -> t -> bool

       (* [evict st t] is called when [t] is no longer sync *)
       val evict : st -> t -> unit
     end) =
struct

  type t =
    | Zero of P.t
    | More of (P.t * frame * t)

  let empty state = Zero (P.empty state)

  let value (Zero p | More (p,_,_)) = p

  let rec drop state n = function
    | Zero _ as t   -> t
    | t when n <= 0 -> t
    | More (p,_,t)  ->
      P.evict state p;
      drop state (n - 1) t

  let size = function
    | Zero _ -> 0
    | More (_,f,_) ->
      (* Frames are indexed starting from 0, so total size is depth + 1 *)
      Frame.depth f + 1

  (* state: data passed to user functor
   * top: top of the current stack
   * pre: computed metric for a previous version of the stack
   *)
  let update state top int =
    (* How many steps were computed in previous integratio ? *)
    let size' = size int in
    (* Chop the top part of previous larger than current stack *)
    let int = drop state (size' - (Frame.depth top + 1)) int in
    (* Conversely, if stack is larger than previous, extract all exceeding
       frames *)
    let rec fat_free acc f =
      if Frame.depth f >= size' then
        match Frame.next f with
        | None -> None, (f :: acc)
        | Some f' -> fat_free (f :: acc) f'
      else Some f, acc
    in
    (* The new top, if any, and all removed frames to be processed *)
    let top, worklist = fat_free [] top in
    (* A valid top should now match previous size *)
    assert (size int - 1 = Option.value_map ~f:Frame.depth ~default:(-1) top);
    (* Decrease previous result and stack until we find matching frames *)
    let rec rewind worklist old top int =
      match int with
      | More (v, last, int')
        when not (Frame.eq top last) || not (P.validate state v) ->
        P.evict state v;
        begin match Frame.next top, int' with
          | None, Zero v' ->
            P.evict state v';
            worklist, None,
            (* Either reuse int' (more efficient ?)
               or regenerate fresh empty value
               or maybe validate previous one before ? *)
            (empty state)
          | None, More _ -> assert false
          | Some top', _   ->
            rewind (top :: worklist) (Some (v,last)) top' int'
        end
      | int -> worklist, old, int
    in
    let worklist, int =
      match top with
      | None -> worklist, (empty state)
      | Some frame ->
        match rewind worklist None frame int with
        | work :: worklist', Some old, int ->
          worklist', More (P.delta state work (value int) ~old, work, int)
        | worklist, _, int ->
          worklist, int
    in
    List.fold_left' worklist
      ~init:int
      ~f:(fun frame int ->
          More (P.frame state frame (value int), frame, int))

  let update' st p t =
    update st (Frame.stack p) t

  let previous = function
    | Zero _ -> None
    | More (_,_,t) -> Some t

  let modify f = function
    | Zero v -> Zero (f v)
    | More (v,s,t) -> More (f v, s, t)
end

let find_marker t =
  let is_rec frame = match Frame.value frame with
    | P.T_ (P.T_REC, ()) -> true
    | P.N_ (P.N_rec_flag, Asttypes.Recursive) -> true
    | P.N_ (P.N_class_fields, _) -> true
    | P.N_ (P.N_class_declarations, _) -> true
    | P.N_ (P.N_class_descriptions, _) -> true
    | _ -> false
  in
  let end_top frame = match Frame.value frame with
    | P.N_ (P.N_structure_item, _) -> true
    | P.N_ (P.N_structure, _) -> true
    | P.N_ (P.N_signature_item, _) -> true
    | P.N_ (P.N_signature, _) -> true
    | P.T_ (P.T_ENTRYPOINT, ()) -> true
    | _ -> false
  in
  let rec find_rec acc = function
    | None -> acc
    | Some frame ->
      find_rec (if is_rec frame then frame else acc) (Frame.next frame)
  in
  let rec find_first acc = function
    | None -> acc
    | Some frame when end_top frame -> find_rec acc (Frame.next frame)
    | Some frame -> find_first frame (Frame.next frame)
  in
  let frame = stack t in
  if Frame.value frame <> P.T_ (P.T_ENTRYPOINT, ()) then
    Some (find_first frame (Some frame))
  else
    None


let has_marker t f' =
  let d = Frame.depth f' in
  if d = 0 then
    false
  else
    let rec aux = function
      | None -> false
      | Some f when Frame.eq f f' -> true
      | Some f when Frame.depth f <= d -> false
      | Some f -> aux (Frame.next f)
    in
    aux (Some (stack t))

let has_marker ?diff t f' =
  match diff with
  | None -> has_marker t f'
  | Some (t',result) ->
    let s, s' = stack t, stack t' in
    let d_inf = min (Frame.depth s) (Frame.depth s') - 1 in
    let d' = Frame.depth f' in
    if d_inf > d' then
      result
    else
      has_marker t f'

let accepting (Parser (raw_parser,_) as parser) =
  let loc_start,t,loc_end = raw_parser.P.env.E.token in
  match feed (loc_end,Raw_parser.EOF,loc_end) parser with
  | `Accept (Raw_parser.N_ (Raw_parser.N_implementation, str)) ->
    `str (str : Parsetree.structure)
  | `Accept (Raw_parser.N_ (Raw_parser.N_interface, sg)) ->
    `sg (sg : Parsetree.signature)
  | _ -> `No
