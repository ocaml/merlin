open Std

module Values = Merlin_parser_values

module P = Raw_parser
module E = MenhirLib.EngineTypes

type state = Raw_parser.state

type t = Raw_parser.feed Raw_parser.parser * MenhirUtils.witness
type parser = t

type frame = int * (Raw_parser.state, Raw_parser.semantic_value) E.stack

let implementation = Raw_parser.implementation_state
let interface = Raw_parser.interface_state

let get_stack s = s.Raw_parser.env.E.stack

let rec of_step s depth =
  match Raw_parser.step s with
  | `Accept _ as a -> a
  | `Reject -> `Reject s
  | `Feed p -> `Step (p, MenhirUtils.stack_depth ~hint:depth (get_stack p))
  | `Step p -> of_step p (MenhirUtils.stack_depth ~hint:depth (get_stack p))

let from state input =
  match of_step (Raw_parser.initial state input) MenhirUtils.initial_depth with
  | `Step p -> p
  | _ -> assert false

let feed (s,t,e as input) (p, depth as parser) =
  match t with
  (* Ignore comments *)
  | Raw_parser.COMMENT _ -> `Step parser
  | _ ->
    let p' = Raw_parser.feed p input in
    of_step p' depth

module Frame : sig
  val stack : t -> frame option
  val depth : frame -> int

  val value : frame -> Raw_parser.semantic_value
  val location : frame -> Location.t
  val eq    : frame -> frame -> bool
  val next  : frame -> frame option

  (* Ease pattern matching on parser stack *)
  type destruct = D of Raw_parser.semantic_value * destruct lazy_t
  val destruct: frame -> destruct
end = struct
  let frame_of d stack = if stack.E.next == stack then None else Some (d,stack)

  let stack_depth (_,w) = MenhirUtils.depth w - 1
  let stack (s,_ as stk) = frame_of (stack_depth stk) (get_stack s)

  let depth (d,f) = d

  let value (_,frame) = frame.E.semv
  let location (_,frame) =
    { Location.
      loc_start = frame.E.startp;
      loc_end = frame.E.endp;
      loc_ghost = false
    }

  let eq (_,f) (_,f') = f == f'
  let next (d,f) = frame_of (d - 1) f.E.next

  (* Ease pattern matching on parser stack *)
  type destruct = D of Raw_parser.semantic_value * destruct lazy_t
  let rec destruct_bottom = D (Raw_parser.Bottom, lazy destruct_bottom)

  let rec destruct f =
    let v = value f in
    let tl = match next f with
      | Some f' -> lazy (destruct f')
      | None -> lazy destruct_bottom
    in
    D (v,tl)
end

let to_step (step,_) = step

let dump ppf t =
  let rec aux ppf = function
    | None -> Format.fprintf ppf "[]\n%!"
    | Some frame ->
      Format.fprintf ppf "(%d, %s) :: %a"
        (Frame.depth frame) (Values.Value.to_string (Frame.value frame))
        aux (Frame.next frame)
  in
  aux ppf (Frame.stack t)

let pop (p, depth) =
  match MenhirUtils.pop p.Raw_parser.env with
  | None -> None
  | Some env ->
    let p = {p with Raw_parser.env = env} in
    Some (p, MenhirUtils.stack_depth ~hint:depth (get_stack p))

let location t =
  match Frame.stack t with
  | None -> Location.none
  | Some frame -> Frame.location frame

let last_token (parser,_) =
  let loc_start,t,loc_end = parser.Raw_parser.env.E.token in
  Location.mkloc t
    { Location. loc_start; loc_end; loc_ghost = false }

let recover ?location flag t =
  match Frame.stack t with
  | None -> None
  | Some frame ->
    let l = match location with
      | None -> Frame.location frame
      | Some l -> l
    in
    match feed (l.Location.loc_start,P.RECOVER flag,l.Location.loc_end) t with
    | `Accept _ -> None
    | `Reject _ -> None
    | `Step t -> Some (Location.mkloc t l)

let reconstruct exn t =
  match Frame.stack t with
  | None -> None
  | Some frame ->
    let {Location. loc_start; loc_end} = Frame.location frame in
    match feed (loc_start,P.RECONSTRUCT exn,loc_end) t with
    | `Accept _ -> None
    | `Reject _ -> None
    | `Step t -> Some t

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

  let rec drop st n = function
    | Zero _ as t   -> t
    | t when n <= 0 -> t
    | More (p,_,t)  ->
      P.evict st p;
      drop st (n - 1) t

  let empty p = Zero (P.empty p)

  let value (Zero p | More (p,_,_)) = p

  let update st frame t =
    let d' = match t with More (_,f,_) -> Frame.depth f | Zero _ -> 0 in
    let t = drop st (d' - Frame.depth frame) t in
    let rec seek acc f =
      if Frame.depth f > d' then
        begin
          Logger.debugf `internal
            (fun ppf (a,b) ->
               Format.fprintf ppf "depth f = %d > d' = %d\n%!" a b)
            (Frame.depth f, d');
          match Frame.next f with
          | None -> assert false
          | Some f' -> seek (f :: acc) f'
        end
      else acc, f
    in
    let ws, frame = seek [] frame in
    let rec rewind acc old f = function
      | More (p', f', ts) when not (Frame.eq f' f) || not (P.validate st p') ->
        P.evict st p';
        begin match Frame.next f, ts with
          | None, Zero p ->
            P.evict st p;
            acc, Some (p',f'),
            (* Either reuse ts (more efficient ?)
               or regenerate fresh empty value
               or maybe validate previous one before ? *)
            (empty st)
          | None, More _ -> assert false
          | Some f', _   ->
            rewind (f :: acc) (Some (p',f')) f' ts
        end
      | t -> acc, old, t
    in
    let ws, t =
      match rewind ws None frame t with
      | f :: ws, Some old, t ->
        ws, More (P.delta st f (value t) ~old, f, t)
      | ws, _, t -> ws, t
    in
    List.fold_left' ~f:(fun f t -> More (P.frame st f (value t), f, t)) ~init:t ws

  let update' st p t =
    match Frame.stack p with
    | None -> empty st
    | Some frame -> update st frame t
end

module Path : sig

  type item =
    | Let of Asttypes.rec_flag * int
    | Struct of int
    | Sig of int
    | Module_rec of int
    | Object of int
    | Class of int

  type path = item list

  type t
  val empty : t
  val update : frame -> t -> t
  val update' : parser -> t -> t

  val get : t -> path
  val length : t -> int
end = struct

  type item =
    | Let of Asttypes.rec_flag * int
    | Struct of int
    | Sig of int
    | Module_rec of int
    | Object of int
    | Class of int

  type path = item list

  module P = struct
    open Raw_parser

    type st = unit
    type t = int * item list
    let empty () = 0, []

    let frame () f (d,p as t) =
      let open Frame in
      match Frame.destruct f, p with
      (* struct _ ... end *)
      | D (Nonterminal (NT'structure_item l),
           lazy ( D (Nonterminal (NT'structure_item _), _)
                | D (Terminal SEMISEMI,
                     lazy (D (Nonterminal (NT'structure_item _), _))))),
        (Struct n :: p') ->
        (d, Struct (List.length l + n) :: p')

      | D (Nonterminal (NT'structure_item l), _), _ ->
        (d + 1, Struct (List.length l) :: p)

      (* sig _ ... end *)
      | D (Nonterminal (NT'signature l), _), _ ->
        (d + 1, Sig (List.length l) :: p)

      (* object ... end *)
      | D (Nonterminal (NT'class_fields l), _), _ ->
        (d + 1, Object (List.length l) :: p)

      (* classes (class _ and ... *)
      | D (Nonterminal (NT'class_declarations l), _), _ ->
        (d + 1, Class (List.length l) :: p)
      | D (Nonterminal (NT'class_descriptions l), _), _ ->
        (d + 1, Class (List.length l) :: p)

      (* let [rec] _ and ... *)
      | D (Nonterminal (NT'let_binding _ | NT'val_ident _ | NT'pattern _),
           lazy (D (Nonterminal (NT'rec_flag flag), _))), _ ->
        (d + 1, Let (flag, 0) :: p)

      | D (Nonterminal (NT'let_bindings l),
           lazy (D (Nonterminal (NT'rec_flag flag), _))), _ ->
        (d + 1, Let (flag, List.length l) :: p)

      | D (Nonterminal (NT'let_bindings l), _), _ ->
        (d + 1, Let (Asttypes.Default, List.length l) :: p)

      (* Module rec *)
      | D (Terminal REC, lazy (D (Terminal MODULE, _))), _ ->
        (d, Module_rec 0 :: p)
      | D (Nonterminal (NT'module_rec_bindings l), _), (Module_rec 0 :: p') ->
        (d, Module_rec (List.length l) :: p')
      | D (Nonterminal (NT'module_rec_declarations l), _), (Module_rec 0 :: p') ->
        (d, Module_rec (List.length l) :: p')
      | D (Nonterminal (NT'module_rec_bindings l), _), _ ->
        (d + 1, Module_rec (List.length l) :: p)
      | D (Nonterminal (NT'module_rec_declarations l), _), _ ->
        (d + 1, Module_rec (List.length l) :: p)

      | _ -> t

    let rec dlength n l' = function
      | l_ when l_ == l' -> n
      | _ :: l_ -> dlength (succ n) l' l_
      | [] -> n

    let delta () f t ~old:(t',f') =
      match Frame.value f', Frame.value f, t' with
      | Nonterminal (NT'signature l'),
        Nonterminal (NT'signature l),
        (d, Sig n' :: p') -> (d, Sig (dlength n' l' l) :: p')
      | Nonterminal (NT'module_rec_bindings l'),
        Nonterminal (NT'module_rec_bindings l),
        (d, Module_rec n' :: p') -> (d, Module_rec (dlength n' l' l) :: p')
      | Nonterminal (NT'module_rec_declarations l'),
        Nonterminal (NT'module_rec_declarations l),
        (d, Module_rec n' :: p') -> (d, Module_rec (dlength n' l' l) :: p')
      | Nonterminal (NT'class_declarations l'),
        Nonterminal (NT'class_declarations l),
        (d, Class n' :: p') -> (d, Class (dlength n' l' l) :: p')
      | Nonterminal (NT'class_descriptions l'),
        Nonterminal (NT'class_descriptions l),
        (d, Class n' :: p') -> (d, Class (dlength n' l' l) :: p')
      | Nonterminal (NT'class_fields l'),
        Nonterminal (NT'class_fields l),
        (d, Object n' :: p') -> (d, Object (dlength n' l' l) :: p')
      | _ -> frame () f t

    let validate () _ = true
    let evict () _ = ()
  end
  module I = Integrate (P)

  type t = I.t
  let empty = I.empty ()
  let update f t = I.update () f t
  let update' p t = I.update' () p t

  let get p = snd (I.value p)
  let length p = fst (I.value p)
end
type path = Path.path
