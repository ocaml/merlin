open Std
open Raw_parser

let extract_annots = function
  | CT_ (_,annots) -> annots
  | CN_ (_,annots) -> annots

let production_annotations prod =
  let sym, def = Query.production_definition prod in
  let head = Option.value_map ~default:[] ~f:extract_annots sym in
  let tail = List.map ~f:extract_annots def in
  head :: tail

let production_annotations =
  Array.memoize Raw_parser.Query.productions ~f:production_annotations

let item_annotations (prod,pos) =
  let annots = production_annotations prod in
  let annots, tail = List.split_n (succ pos) annots in
  let expected = List.drop_n pos (snd (Query.production_definition prod)) in
  let expected = match expected with
    | [] -> []
    | hd :: _ -> [hd]
  in
  expected, List.rev annots

let rec merge_annotations acc = function
  | [] ->
    begin match acc with
    | last :: body -> List.rev body, last
    | _ -> assert false
    end
  | lists ->
    let hd' = function hd :: _ -> Some hd | [] -> None in
    let tl' = function _ :: [] | [] -> None | _ :: tl -> Some tl in
    let hds = List.filter_map ~f:hd' lists in
    let tls = List.filter_map ~f:tl' lists in
    let hds = List.concat hds in
    merge_annotations (hds :: acc) tls
let merge_annotations annots = merge_annotations [] annots

let itemset_annotations lr0 =
  let itemset = Raw_parser.Query.itemset lr0 in
  let annots = List.map ~f:item_annotations itemset in
  let expected, annots = List.split annots in
  List.concat expected, merge_annotations annots

let itemset_annotations =
  Array.memoize Raw_parser.Query.lr0_states ~f:itemset_annotations

type explanation = {
  item: (string * Merlin_parser.frame) option;
  unclosed: (string * Merlin_parser.frame) option;
  expected: Raw_parser.symbol_class list;
}

let rec annotate last frame =
  let open Merlin_parser in
  let body, last' =
    match snd (itemset_annotations (Frame.lr0_state frame)) with
    | [], last' -> [last], last'
    | hd :: tl, last' -> (last @ hd) :: tl, last'
  in
  let rec concat frame annot = match frame, annot with
    | None, _ -> lazy List.Lazy.Nil
    | Some frame, (hd :: tl) ->
      let tl = concat (Frame.next frame) tl in
      let result = List.Lazy.Cons ((frame , hd), tl) in
      lazy result
    | Some frame, [] ->
      annotate last' frame
  in
  concat (Some frame) body

let annotate frame =
  Lazy.force (annotate [] frame)

let explain parser =
  let frame = Merlin_parser.stack parser in
  let item = ref None in
  let unclosed = ref None in
  let closed = ref 0 in
  let process_annot result = function
    | `Item name when !item = None -> item := Some (name, result)
    | `Close -> incr closed
    | `Unclosed _ when !closed > 0 -> decr closed
    | `Unclosed name when !unclosed = None -> unclosed := Some (name, result)
    | _ -> ()
  in
  let rec extract = function
    | List.Lazy.Cons ((frame, annots), next) ->
      List.iter ~f:(process_annot frame) annots;
      begin match !item with
      | None -> extract (Lazy.force next)
      | Some _ -> ()
      end
    | List.Lazy.Nil -> ()
  in
  extract (annotate frame);
  let expected, _ =
    itemset_annotations (Merlin_parser.get_lr0_state parser) in
  { item = !item; unclosed = !unclosed; expected }

