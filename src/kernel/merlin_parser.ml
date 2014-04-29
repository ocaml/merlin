open Std

module Values = Raw_parser_values

module P = Raw_parser
module E = MenhirLib.EngineTypes

type state = Raw_parser.state

type t =
  | Partial of Raw_parser.feed Raw_parser.parser * MenhirUtils.witness
  | Final of Raw_parser.semantic_value Location.loc

type parser = t

type frame =
  | Partial_frame of int * (Raw_parser.state, Raw_parser.semantic_value) E.stack
  | Final_frame of Raw_parser.semantic_value Location.loc

let get_stack s = s.Raw_parser.env.E.stack
let mk_loc loc_start loc_end = {Location. loc_start; loc_end; loc_ghost = false}

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

  let frame_of d stack =
    if stack.E.next == stack then
      None
    else
      Some (Partial_frame (d,stack))

  let stack = function
    | Partial (parser,w) ->
      frame_of (MenhirUtils.depth w - 1) (get_stack parser)
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

let implementation = Raw_parser.implementation_state
let interface = Raw_parser.interface_state

let stack = Frame.stack
let location t =
  match stack t with
  | None -> Location.none
  | Some frame -> Frame.location frame

let reached_eof = function
  | Partial _ -> false
  | Final _   -> true

let rec of_step s depth =
  match Raw_parser.step s with
  | `Accept txt ->
    let frame = (get_stack s) in
    let loc = mk_loc frame.E.startp frame.E.endp in
    `Step (Final {Location. txt; loc})
  | `Reject -> `Reject
  | `Feed p ->
    `Step (Partial (p, MenhirUtils.stack_depth ~hint:depth (get_stack p)))
  | `Step p ->
    of_step p (MenhirUtils.stack_depth ~hint:depth (get_stack p))

let from state input =
  match of_step (Raw_parser.initial state input) MenhirUtils.initial_depth with
  | `Step p -> p
  | _ -> assert false

let feed (s,t,e as input) parser =
  match parser with
  | Final _ -> `Reject
  | Partial (p, depth) ->
    match t with
    (* Ignore comments *)
    | Raw_parser.COMMENT _ -> `Step parser
    | _ ->
      let p' = Raw_parser.feed p input in
      of_step p' depth

let to_step = function
  | Partial (step,_) -> Some step
  | Final _ -> None

let dump ppf t =
  let rec aux ppf = function
    | None -> Format.fprintf ppf "[]\n%!"
    | Some frame ->
      Format.fprintf ppf "(%d, %s) :: %a"
        (Frame.depth frame) (Values.to_string (Frame.value frame))
        aux (Frame.next frame)
  in
  aux ppf (Frame.stack t)

let pop = function
  | Final _ -> None
  | Partial (p, depth) ->
    match MenhirUtils.pop p.Raw_parser.env with
    | None -> None
    | Some env ->
      let p = {p with Raw_parser.env = env} in
      Some (Partial (p, MenhirUtils.stack_depth ~hint:depth (get_stack p)))

let last_token = function
  | Final {Location. loc = {Location. loc_end = l; _}; _} ->
    Location.mkloc Raw_parser.EOF
      {Location. loc_start = l; loc_end = l; loc_ghost = false}
  | Partial (parser,_) ->
    let loc_start,t,loc_end = parser.Raw_parser.env.E.token in
    Location.mkloc t
      {Location. loc_start; loc_end; loc_ghost = false}

let recover ?location t =
  match Frame.stack t with
  | None -> None
  | Some frame ->
    let l = match location with
      | None -> Frame.location frame
      | Some l -> l
    in
    let t =
      match feed (l.Location.loc_start,P.DEFAULT,l.Location.loc_end) t with
      | `Accept _ -> None
      | `Reject -> None (*Option.bind (pop t) ~f:pop*)
      | `Step t -> Some t
    in
    match t with
    | None -> None
    | Some t -> Some (Location.mkloc t l)

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
        begin
          Logger.debugf `internal
            (fun ppf (a,b) ->
               Format.fprintf ppf "depth f = %d >= size = %d\n%!" a b)
            (Frame.depth f, size');
          match Frame.next f with
          | None -> None, (f :: acc)
          | Some f' -> fat_free (f :: acc) f'
        end
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
        (d + 1, Let (Asttypes.Nonrecursive, List.length l) :: p)

      (* Module rec *)
      | D (Terminal REC, lazy (D (Terminal MODULE, _))), _ ->
        (d, Module_rec 0 :: p)
      (*| D (Nonterminal (NT'module_rec_bindings l), _), (Module_rec 0 :: p') ->
        (d, Module_rec (List.length l) :: p')*)
      | D (Nonterminal (NT'module_rec_declarations l), _), (Module_rec 0 :: p') ->
        (d, Module_rec (List.length l) :: p')
      (*| D (Nonterminal (NT'module_rec_bindings l), _), _ ->
        (d + 1, Module_rec (List.length l) :: p)*)
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
      (*| Nonterminal (NT'module_rec_bindings l'),
        Nonterminal (NT'module_rec_bindings l),
        (d, Module_rec n' :: p') -> (d, Module_rec (dlength n' l' l) :: p')*)
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
