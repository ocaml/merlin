open Std
open Raw_parser

exception Partial_stack of Merlin_parser.frame list

let diff ~stack:p1 ~wrt:p2 =
  let open Merlin_parser in
  let rec extract o1 o2 acc =
    match o1, o2 with
    | None, _ | _, None -> acc
    | Some f1, Some f2 ->
      let d1, d2 = Frame.depth f1, Frame.depth f2 in
      if d1 < d2 then
        extract o1 (Frame.next f2) (f2 :: acc)
      else if d1 > d2 then
        extract (Frame.next f1) o2 acc
      else if Frame.eq f1 f2 then
        acc
      else
        extract (Frame.next f1) (Frame.next f2) (f2 :: acc)
  in
  extract (Frame.stack p1) (Frame.stack p2) []

let extract_nt l =
  let f frame =
    match Merlin_parser.Frame.value frame with
    | Nonterminal nt -> Some nt
    | _ -> None
  in
  List.filter_map ~f l

let rec reconstruct_struct = function
  | (NT'structure_item x | NT'structure_tail x | NT'structure x) :: tl ->
    x @ reconstruct_struct tl
  | _ :: tl -> reconstruct_struct tl
  | [] -> []

let () = Parsing_aux.reconstruct_struct :=
           (function
             | Partial_stack frames ->
               reconstruct_struct (extract_nt frames)
             | _ -> [])
