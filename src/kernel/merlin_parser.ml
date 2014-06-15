open Std

module Values = Raw_parser_values

module P = Raw_parser
module E = MenhirLib.EngineTypes

type state = Raw_parser.state

type t =
  | Partial of P.feed P.parser * MenhirUtils.depth
  | Final of P.symbol Location.loc

type parser = t

type frame =
  | Partial_frame of int * (P.state, P.symbol) E.stack
  | Final_frame of P.symbol Location.loc

let get_stack s = s.P.env.E.stack
let get_state s = s.P.env.E.current
let mk_loc loc_start loc_end = {Location. loc_start; loc_end; loc_ghost = false}

module Frame : sig
  val stack : t -> frame option
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
      Some (Partial_frame (d,stack))

  let stack = function
    | Partial (parser,w) ->
      frame_of (MenhirUtils.Depth.get w - 1) (get_stack parser)
    | Final result ->
      Some (Final_frame result)

  let depth = function
    | Partial_frame (d,f) -> d
    | Final_frame _ -> 0

  let value = function
    | Partial_frame (_,frame) -> frame.E.semv
    | Final_frame l -> l.Location.txt

  let location = function
    | Partial_frame (_,frame) ->
      mk_loc frame.E.startp frame.E.endp
    | Final_frame l ->
      l.Location.loc

  let eq a b = match a, b with
    | Partial_frame (_,f), Partial_frame (_,f') -> f == f'
    | Final_frame f,       Final_frame f'       -> f == f'
    | _ -> false

  let next = function
    | Partial_frame (d,f) -> frame_of (d - 1) f.E.next
    | Final_frame _ -> None

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

let pop = function
  | Final _ -> None
  | Partial (p, depth) ->
    match MenhirUtils.pop p.P.env with
    | None -> None
    | Some env ->
      let p = {p with P.env = env} in
      Some (Partial (p, MenhirUtils.Depth.stack ~hint:depth (get_stack p)))

let location t =
  Option.value_map
    ~default:Location.none
    ~f:Frame.location
    (stack t)

let reached_eof = function
  | Partial _ -> false
  | Final _   -> true

let of_feed p depth =
  Partial (p, MenhirUtils.Depth.stack ~hint:depth (get_stack p))

let rec of_step s depth =
  match P.step s with
  | `Accept txt ->
    let frame = (get_stack s) in
    let loc = mk_loc frame.E.startp frame.E.endp in
    `Step (Final {Location. txt; loc})
  | `Reject -> `Reject
  | `Feed p -> `Step (of_feed p depth)
  | `Step p -> of_step p (MenhirUtils.Depth.stack ~hint:depth (get_stack p))

let to_step = function
  | Partial (step,_) -> Some step
  | Final _ -> None

let from state input =
  match of_step (P.initial state input) MenhirUtils.Depth.initial with
  | `Step p -> p
  | _ -> assert false

let feed (s,t,e as input) parser =
  match parser with
  | Final _ -> `Reject
  | Partial (p, depth) ->
    match t with
    (* Ignore comments *)
    | P.COMMENT _ -> `Step parser
    | _ ->
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
  Format.fprintf ppf "itemset:\n";
  List.iter ~f:(Format.fprintf ppf "- %a\n" dump_item) l

let dump ppf t =
  (* Print current frame, with its itemset *)
  begin match t with
    | Partial (s,_) ->
      let state = get_state s in
      let itemset = P.Query.itemset state in
      dump_itemset ppf itemset
    | Final _ -> ()
  end;
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
  aux true ppf (Frame.stack t);
  Format.fprintf ppf "]\n%!"

let last_token = function
  | Final {Location. loc = {Location. loc_end = l; _}; _} ->
    Location.mkloc P.EOF
      {Location. loc_start = l; loc_end = l; loc_ghost = false}
  | Partial (parser,_) ->
    let loc_start,t,loc_end = parser.P.env.E.token in
    Location.mkloc t
      {Location. loc_start; loc_end; loc_ghost = false}

let recover t = match t with
  | Final _ -> None
  | Partial (p,w) ->
    let env = p.P.env in
    let lr1_state = env.E.current in
    let lr0_state = P.Query.lr0_state lr1_state in
    let strategies = Merlin_recovery_strategy.reduction_strategy lr0_state in
    Logger.errorf `parser (fun ppf strategies ->
        Format.fprintf ppf "search for strategies at %d.\n" lr0_state;
        dump_itemset ppf (P.Query.itemset lr0_state);
        if strategies = [] then
          Format.fprintf ppf "no candidate selected, dropping.\n"
        else
          begin
            Format.fprintf ppf "candidates: (selected in first position)\n";
            List.iter strategies
              ~f:(fun (cost,l,prod,_) ->
                  let l = List.map ~f:Values.class_of_symbol l in
                  let l = List.map ~f:Values.string_of_class l in
                  Format.fprintf ppf
                    "- at cost %d, production %d, %s\n" cost prod
                    (String.concat " " l)
                )
          end
      ) strategies;
    match strategies with
    | [] -> None
    | (_, symbols, prod, action) :: _ ->
      let add_symbol stack symbol =
        {stack with E. semv = symbol; startp = stack.E.endp; next = stack}
      in
      (* Feed stack *)
      let stack = List.fold_left ~f:add_symbol ~init:env.E.stack symbols in
      let env = {env with E. stack} in
      (* Reduce stack *)
      (* FIXME: action can raise an error. We should catch it and fallback to
         another strategy *)
      let stack = action env in
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
      let env = {env with E. current = goto stack.E.state prod} in

      (* Construct parser *)
      Some (of_feed {p with P. env} w)

let mark f r = function
  | Final _ as t -> r := false; t
  | Partial (p,d) ->
    let d' = Frame.depth f in
    Partial (p, MenhirUtils.Depth.mark (d' + 1) (fun () -> r := false) d)
    

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
    match Frame.stack p with
    | None -> empty st
    | Some frame -> update st frame t

  let previous = function
    | Zero _ -> None
    | More (_,_,t) -> Some t

  let modify f = function
    | Zero v -> Zero (f v)
    | More (v,s,t) -> More (f v, s, t)
end
