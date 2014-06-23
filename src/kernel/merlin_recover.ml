open Std
open Raw_parser

let rollbacks endp parser =
  let rec aux (termination,parser) =
    (* FIXME: find proper way to handle limit conditions *)
    (* When reaching bottom of the stack, last frame will raise an Accept
       exception, we can't recover from it, and we shouldn't recover TO it. *)
    try
      match Merlin_parser.recover ~endp termination parser with
      | Some _ as r -> r
      | None ->
        Option.map ~f:(fun a -> Merlin_parser.termination, a)
          (Merlin_parser.pop parser)
    with _ -> None
  in
  let parser = Merlin_parser.termination, parser in
  let stacks = parser :: List.unfold aux parser in
  let stacks = List.rev_map stacks ~f:snd in
  (* Hack to drop last parser *)
  let stacks = List.rev (List.tl stacks) in
  Zipper.of_list stacks

type t = {
  errors: exn list;
  parser: Merlin_parser.t;
  recovering: (Merlin_parser.t zipper) option;
}

let parser t = t.parser
let exns t = t.errors

let fresh parser = {errors = []; parser; recovering = None}

let rec feed_normal (s,tok,e as input) parser =
  Logger.debugf `internal
    (fun ppf tok -> Format.fprintf ppf "normal parser: received %s"
        Merlin_parser.Values.(string_of_class (class_of_symbol (symbol_of_token tok))))
    tok;
  match Merlin_parser.feed input parser with
  | `Accept _ ->
    Logger.debug `internal "parser accepted";
    assert (tok = EOF);
    feed_normal (s,SEMISEMI,e) parser
  | `Reject ->
    Logger.debug `internal "parser rejected";
    None
  | `Step parser ->
    Some parser

let closing_token = function
  | END -> true
  | RPAREN -> true
  | _ -> false

let prepare_candidates candidates =
  let open Location in
  let candidates = List.rev candidates in
  (*let candidates = List.group_by
      (fun (a : _ loc) (b : _ loc) ->
        Lexing.compare_pos a.loc.loc_start b.loc.loc_start = 0)
      candidates
  in*)
  let parser_priority p =
    let symbol = Merlin_parser.Frame.value (Merlin_parser.stack p) in
    let symcls = Merlin_parser.Values.class_of_symbol symbol in
    Raw_parser_values.selection_priority symcls
  in
  let cmp pa pb =
    - compare (parser_priority pa) (parser_priority pb)
  in
  (*List.concat_map (List.stable_sort ~cmp) candidates*)
  List.stable_sort ~cmp candidates


let feed_recover original (s,tok,e as input) zipper =
  let get_col x = snd (Lexing.split_pos x) in
  let ref_col = get_col s in
  (* Find appropriate recovering position *)
  let less_indented p =
    let loc = Merlin_parser.location p in
    get_col loc.Location.loc_start <= ref_col
  and more_indented p =
    let loc = Merlin_parser.location p in
    get_col loc.Location.loc_start >= ref_col
  in
  (* Backward: increase column *)
  (* Forward: decrease column *)
  let zipper = Zipper.seek_forward more_indented zipper in
  let zipper = Zipper.seek_backward less_indented zipper in
  let candidates = prepare_candidates (Zipper.select_forward more_indented zipper) in
  Logger.errorf `protocol (fun ppf candidates->
    Format.fprintf ppf "recovery candidates starting at %a:\n%!"
      Lexing.print_position s;
    let dump_snapshot p =
      Format.fprintf ppf "- %a\n"
        Merlin_parser.dump p
    in
    List.iter dump_snapshot candidates)
    candidates;
  let rec aux_feed = function
    | [] -> Either.L zipper
    | candidate :: candidates ->
      aux_dispatch candidates candidate
        (Merlin_parser.feed input candidate)

  and aux_dispatch candidates candidate = function
    | `Step parser ->
      Logger.debugf `internal (fun ppf ->
          Format.fprintf ppf "selected recovery %a\n%!" Merlin_parser.dump)
        candidate;
      Either.R parser
    | `Accept _ ->
      Logger.debugf `internal (fun ppf ->
          Format.fprintf ppf "accepted recovery %a\n%!" Merlin_parser.dump)
        candidate;
      assert (tok = EOF);
      aux_dispatch candidates candidate
        (Merlin_parser.feed (s,SEMISEMI,e) candidate)
    | `Reject ->
      Logger.debugf `internal (fun ppf ->
          Format.fprintf ppf "failed recovery from %a\n%!" Merlin_parser.dump)
        candidate;
      aux_feed candidates

  in
  aux_feed candidates

let fold warnings token t =
  match token with
  | Merlin_lexer.Error _ -> t
  | Merlin_lexer.Valid (s,tok,e) ->
    Logger.debugf `internal
      (fun ppf tok -> Format.fprintf ppf "received %s"
          Merlin_parser.Values.(string_of_class (class_of_symbol (symbol_of_token tok))))
      tok;
    Logger.debugf `internal Merlin_parser.dump t.parser;
    warnings := [];
    let pop w = let r = !warnings in w := []; r in
    let recover_from t recovery =
      match feed_recover t.parser (s,tok,e) recovery with
      | Either.L recovery ->
        {t with recovering = Some recovery}
      | Either.R parser ->
        {t with parser; recovering = None}
    in
    match t.recovering with
    | Some recovery -> recover_from t recovery
    | None ->
      begin match feed_normal (s,tok,e) t.parser with
        | None ->
          let recovery = rollbacks e t.parser in
          let step = Merlin_parser.to_step t.parser in
          let error = Error_classifier.from step (s,tok,e) in
          recover_from
            {t with errors = error :: (pop warnings) @ t.errors}
            recovery
        | Some parser ->
          {t with errors = (pop warnings) @ t.errors; parser }
      end

let fold token t =
  let warnings = ref [] in
  Either.get (Parsing_aux.catch_warnings warnings
                (fun () -> fold warnings token t))


let dump_recovering ppf = function
  | None -> Format.fprintf ppf "clean"
  | Some (Zipper (head, _, tail)) ->
    let dump_snapshot p =
      Format.fprintf ppf "- %a\n"
        Merlin_parser.dump p
    in
    let iter ppf l = List.iter ~f:dump_snapshot l in
    Format.fprintf ppf "recoverable states\nhead:\n%atail:\n%a"
      iter head
      iter tail

let dump ppf t =
  Format.fprintf ppf "parser: %a\n" Merlin_parser.dump t.parser;
  Format.fprintf ppf "recovery: %a\n" dump_recovering t.recovering

let dump_recoverable ppf t =
  let t = match t.recovering with
    | Some _ -> t
    | None -> {t with recovering = Some (rollbacks Lexing.dummy_pos t.parser)}
  in
  dump ppf t
