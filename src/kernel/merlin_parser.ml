open Std

module Values = Merlin_parser_values

module P = Raw_parser
module E = MenhirLib.EngineTypes

type state = Raw_parser.state

type t =
  | First of state
  | Other of Raw_parser.feed Raw_parser.parser * MenhirUtils.witness

type frame = int * (Raw_parser.state, Raw_parser.semantic_value) E.stack

let implementation = Raw_parser.implementation_state
let interface = Raw_parser.interface_state

let get_stack s = s.Raw_parser.env.E.stack

let rec of_step s depth =
  match Raw_parser.step s with
  | `Accept _ as a -> a
  | `Reject -> `Reject s
  | `Feed p ->
    `Step (Other (p, MenhirUtils.stack_depth ~hint:depth (get_stack p)))
  | `Step p -> of_step p (MenhirUtils.stack_depth ~hint:depth (get_stack p))

let from state = First state

let feed input p =
  let p', depth = match p with
    | First state ->
      Raw_parser.initial state input, MenhirUtils.initial_depth
    | Other (p, depth) -> Raw_parser.feed p input, depth
  in
  of_step p' depth

let frame_of d stack = if stack.E.next == stack then None else Some (d,stack)

let stack = function
  | First _ -> None
  | Other (s,w) -> frame_of (MenhirUtils.depth w) (get_stack s)

let stack_depth = function
  | First _ -> 0
  | Other (_,w) ->(MenhirUtils.depth w)

let depth (d,f) = d

let value (_,frame) = frame.E.semv
let eq (_,f) (_,f') = f == f'
let next (d,f) = frame_of (d - 1) f.E.next

let of_step ?(hint=MenhirUtils.initial_depth) step =
  of_step step MenhirUtils.(stack_depth ~hint (get_stack (step)))

let to_step = function
  | First _ -> None
  | Other (step,_) -> Some step

module Integrate
    (P : sig
       type t
       val empty : t (* Base-case, empty stack *)
       val frame : frame -> t -> t (* Add frame *)
     end) =
struct
  type t = (P.t * frame) list
  let empty = []

  let value = function  ((p,_f) :: _) -> p | [] -> P.empty

  let update frame t =
    let d' = match t with ((_p,f) :: _) -> depth f | [] -> 0 in
    let t = List.drop_n (d' - depth frame) t in
    let rec seek acc f =
      if depth f > d' then
        let f' = match next f with None -> assert false | Some f -> f in
        seek (f :: acc) f'
      else acc, f
    in
    let worklist, frame = seek [] frame in
    let rec rewind acc f = function
      | [] -> assert false
      | (_, f') :: ts when not (eq f' f) ->
        begin match next f with
          | None ->
            assert (ts = []); acc, []
          | Some f' -> rewind (f :: acc) f' ts
        end
      | t -> acc, t
    in
    let worklist, t = rewind worklist frame t in
    List.fold_left' ~f:(fun f t -> ((P.frame f (value t), f) :: t)) ~init:t worklist
end

module Path : sig
  type path =
    | Root
    | Items of int * path
    | Sub of Asttypes.rec_flag * path

  type t
  val empty : t
  val update : frame -> t -> t

  val get : t -> path
  val length : t -> int
end = struct
  type path =
    | Root
    | Items of int * path
    | Sub of Asttypes.rec_flag * path
  module P = struct
    type t = int * path
    let empty = (0,Root)
    let frame f (d,p as t) =
      let items l =
        let m = max 1 (List.length l) in
        function
        | Items (n,p) -> (d, Items (n + m, p))
        | p -> (d + 1, Items (m, p))
      in
      match value f with
      | Raw_parser.Nonterminal (Raw_parser.NT'rec_flag rec') ->
        (d + 1, Sub (rec',p))
      | Raw_parser.Nonterminal (Raw_parser.NT'signature_item l) ->
        items l p
      | Raw_parser.Nonterminal (Raw_parser.NT'structure_item l) ->
        items l p
      | _ -> t
  end
  module I = Integrate (P)
  include I

  let get p = snd (value p)
  let length p = fst (value p)
end
