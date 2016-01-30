open MenhirSdk
open Cmly_format
open Utils
open Synthesis

type item = lr1_state * production * int

type recovery = lr1_state -> int * (lr1_state option * item list) list

let item_to_string (st, prod, p) =
  Printf.sprintf "(#%d, p%d, %d)" st.lr1_index prod.p_index p

module ItemsH = Hashtbl.Make(struct
    type t = item list
    let hash = function
      | (lr1, prod, pos) :: _ ->
          Hashtbl.hash (lr1.lr1_index, prod.p_index, pos)
      | [] -> 0
    let rec equal a b = match a, b with
      | a, b when a == b -> true
      | (l1, p1, x1) :: xs, (l2, p2, x2) :: ys when
          l1 == l2 && p1 == p2 && x1 == x2 ->
          equal xs ys
      | [], [] -> true
      | _ -> false
  end)

module SynthH = Hashtbl.Make(struct
    type t = variable
    let hash = function
      | Head (lr1, nt) ->
          Hashtbl.hash (lr1.lr1_index,nt.n_index)
      | Tail (lr1, prod, pos) ->
          Hashtbl.hash (lr1.lr1_index, prod.p_index, pos)
    let equal a b = match a,b with
      | Head (l1, n1), Head (l2, n2) ->
          l1 == l2 && n1 == n2
      | Tail (l1, p1, x1), Tail (l2, p2, x2) ->
          l1 == l2 && p1 == p2 && x1 == x2
      | _ -> false
  end)

type trace = Trace of float * item list

module Trace = struct
  type t = trace
  let min (Trace (c1, _) as tr1) (Trace (c2, _) as tr2) =
    arg_min_float (fun (Trace (c,_)) -> c) tr1 tr2

  let cat (Trace (c1, tr1)) (Trace (c2, tr2)) =
    Trace (c1 +. c2, tr1 @ tr2)

  let to_string (Trace (c1, tr)) =
    Printf.sprintf "Trace (%f, %s)"
      c1 (list_fmt item_to_string tr)
end

module State = struct
  type level = (nonterminal * Trace.t) list
  type t = level list

  let rec merge_level l1 l2 : level = match l1, l2 with
    | [], l -> l
    | l, [] -> l
    | ((nt1, c1) :: xs1), (x2 :: xs2) ->
      let (nt2, c2) = x2 in
      match compare nt1.n_index nt2.n_index with
      | 0 ->
        let x = (nt1, Trace.min c1 c2) in
        x :: merge_level xs1 xs2
      | n when n > 0 -> x2 :: merge_level l1 xs2
      | _ -> (nt1, c1) :: merge_level xs1 l2

  let rec merge l1 l2 : t = match l1, l2 with
    | [], l -> l
    | l, [] -> l
    | (x1 :: l1), (x2 :: l2) ->
      let x' = merge_level x1 x2 in
      x' :: merge l1 l2

  let reduction_to_string (n, tr) =
    Printf.sprintf "(%s, %s)" n.n_name (Trace.to_string tr)

  let to_string (t : t) = list_fmt (list_fmt reduction_to_string) t
end

module Make (G : Solution) : sig
  val recover : recovery
  val report : Format.formatter -> unit
end = struct

  let synthesize =
    let rec add_nt tr nt = function
      | [] -> [(nt, tr)]
      | x :: xs ->
          let c = compare nt.n_index (fst x).n_index in
          if c = 0 then (nt, Trace.min tr (snd x)) :: xs
          else if c < 0 then
            (nt, tr) :: xs
          else
            x :: add_nt tr nt xs
    in
    let add_item cost item stack =
      let (_, prod, pos) = item in
      if cost = infinity then stack
      else
        let stack_hd = function
          | [] -> []
          | x :: _ -> x
        and stack_tl = function
          | [] -> []
          | _ :: xs -> xs
        in
        let rec aux stack = function
          | 0 -> add_nt (Trace (cost, [item])) prod.p_lhs (stack_hd stack) ::
                 stack_tl stack
          | n -> stack_hd stack :: aux (stack_tl stack) (n - 1)
        in
        aux stack pos
    in
    let table = Array.map (fun st ->
        Array.fold_left (fun acc (prod, pos) ->
            if pos = 0 then (
              (*if prod.p_kind = `START then ( *)
              (* pos = 0 means we are on an initial state *)
              (*report "skipping %s at depth %d\n" prod.p_lhs.n_name pos;*)
              acc
            ) else (
              (*report "adding %s at depth %d\n" prod.p_lhs.n_name pos;*)
              add_item
                (G.cost_of (Tail (st, prod, pos)))
                (st, prod, pos) acc
            )
          )
          [] st.lr1_lr0.lr0_items
      ) G.grammar.g_lr1_states
    in
    fun st -> table.(st.lr1_index)

  let step st ntss =
    let seen = ref CompressedBitSet.empty in
    let rec aux = function
      | [] -> []
      | ((nt, tr) :: x) :: xs
        when not (CompressedBitSet.mem nt.n_index !seen) && not (nt.n_kind = `START) ->
          seen := CompressedBitSet.add nt.n_index !seen;
          let st' = array_assoc st.lr1_transitions (N nt) in
          let xs' = synthesize st' in
          let xs' = match xs' with
            | [] -> []
            | _ :: xs -> xs
          in
          let merge_trace (nt,tr') = (nt, Trace.cat tr' tr) in
          let xs' = List.map (List.map merge_trace) xs' in
          aux (State.merge xs' (x :: xs))
      | (_ :: x) :: xs -> aux (x :: xs)
      | [] :: xs -> xs
    in
    aux ntss

  let init st = ((st, [st.lr1_index]), step st (synthesize st))

  let pred =
    (* Compute lr1 predecessor relation *)
    let tbl1 = Array.make (Array.length G.grammar.g_lr1_states) [] in
    let revert_transition s1 (sym,s2) =
      assert (match s2.lr1_lr0.lr0_incoming with
          | None -> false
          | Some sym' -> sym = sym');
      tbl1.(s2.lr1_index) <- s1 :: tbl1.(s2.lr1_index)
    in
    Array.iter
      (fun lr1 -> Array.iter (revert_transition lr1) lr1.lr1_transitions)
      G.grammar.g_lr1_states;
    (fun lr1 -> tbl1.(lr1.lr1_index))

  let expand ((st, sts), nts) =
    List.map (fun st' -> ((st', st'.lr1_index :: sts), step st' nts)) (pred st)

  let recover st =
    (* How big is the known prefix of the stack *)
    let pos = Array.fold_left (fun pos (_, pos') -> max pos pos')
        (snd st.lr1_lr0.lr0_items.(0)) st.lr1_lr0.lr0_items
    in
    (* Walk this prefix *)
    let traces =
      let acc = ref [init st] in
      for i = 1 to pos - 1 do
        acc := List.concat (List.map expand !acc)
      done;
      !acc
    in
    (* Last step *)
    let select_trace traces =
      (* Pick a trace with minimal cost, somewhat arbitrary *)
      match List.flatten traces with
      | [] -> assert false
      | (_, trace) :: alternatives ->
          List.fold_left
            (fun tr1 (_,tr2) -> Trace.min tr1 tr2)
            trace alternatives
    in
    let process_trace trace =
      match expand trace with
      | [] -> (* Initial state *)
          assert (snd trace = []); []
      | states ->
          let select_expansion ((st, sts), trace') =
            if trace' = [] then
              (* Reached stack bottom *)
              (None, select_trace (snd trace))
            else
              (Some st, select_trace trace')
          in
          List.map select_expansion states
    in
    pos,
    List.flatten @@ List.map (fun trace ->
        List.map
          (fun (st, Trace (_, reductions)) -> st, reductions)
          (process_trace trace)
      ) traces

  let recover =
    let a = Array.map recover G.grammar.g_lr1_states in
    fun lr1 -> a.(lr1.lr1_index)

  let report ppf = ()
end
