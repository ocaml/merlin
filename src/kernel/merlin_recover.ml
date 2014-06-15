open Std
open Raw_parser

let rollbacks parser =
  let rec aux parser =
    (* FIXME: find proper way to handle limit conditions *)
    (* When reaching bottom of the stack, last frame will raise an Accept
       exception, we can't recover from it, and we shouldn't recover TO it. *)
    try
      match Merlin_parser.recover parser with
      | Some _ as r -> r
      | None -> Merlin_parser.pop parser
    with _ -> None
  in
  let stacks = parser :: List.unfold aux parser in
  let stacks = List.rev_map stacks
      ~f:(fun p -> Location.mkloc p (Merlin_parser.location p))
  in
  (* Hack to drop last parser *)
  let stacks = List.rev (List.tl stacks) in
  Zipper.of_list stacks

type t = {
  errors: exn list;
  parser: Merlin_parser.t;
  recovering: (Merlin_parser.t Location.loc zipper) option;
}

let parser t = t.parser
let exns t = t.errors

let fresh parser = {errors = []; parser; recovering = None}

let feed_normal (_,tok,_ as input) parser =
  Logger.debugf `internal
    (fun ppf tok -> Format.fprintf ppf "normal parser: received %s"
        Merlin_parser.Values.(string_of_class (class_of_symbol (symbol_of_token tok))))
    tok;
  match Merlin_parser.feed input parser with
  | `Reject ->
    Logger.debug `internal "parser rejected";
    None
  | `Step parser ->
    Some parser

let closing_token = function
  | END -> true
  | RPAREN -> true
  | _ -> false

let feed_recover original (s,tok,e as input) zipper =
  let get_col x = snd (Lexing.split_pos x) in
  let col = get_col s in
  (* Find appropriate recovering position *)
  let until_after  {Location. txt; loc} = col >= get_col loc.Location.loc_start
  and until_before {Location. txt; loc} = get_col loc.Location.loc_start >= col
  in
  let zipper = Zipper.seek_backward until_before zipper in
  let zipper = Zipper.seek_forward until_after zipper in
  let candidates = List.rev (Zipper.select_backward until_before zipper) in
  let rec aux = function
    | {Location. txt = candidate} :: candidates ->
      begin match Merlin_parser.feed input candidate with
      | `Step parser ->
        Logger.debugf `internal (fun ppf ->
            Format.fprintf ppf "selected recovery %a\n%!" Merlin_parser.dump)
          candidate;
        Either.R parser
      | `Reject ->
        Logger.debugf `internal (fun ppf ->
            Format.fprintf ppf "failed recovery from %a\n%!" Merlin_parser.dump)
          candidate;
        aux candidates
      end
    | [] -> Either.L zipper
  in
  aux candidates

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
          let recovery = rollbacks t.parser in
          begin match Merlin_parser.to_step t.parser with
            | Some step ->
              let error = Error_classifier.from step (s,tok,e) in
              recover_from
                {t with errors = error :: (pop warnings) @ t.errors}
                recovery
            | None -> t
          end
        | Some parser ->
          {t with errors = (pop warnings) @ t.errors; parser }
      end

let fold token t =
  let warnings = ref [] in
  Either.get (Parsing_aux.catch_warnings warnings
                (fun () -> fold warnings token t))

let dump_snapshot ppf {Location. txt; loc} =
  Format.fprintf ppf "- position: %a\n  parser: %a\n"
    Location.print (Merlin_parser.location txt)
    Merlin_parser.dump txt

let dump_recovering ppf = function
  | None -> Format.fprintf ppf "clean"
  | Some (Zipper (head, _, tail)) ->
    let iter ppf l = List.iter ~f:(dump_snapshot ppf) l in
    Format.fprintf ppf "recoverable states\nhead:\n%atail:\n%a"
      iter head
      iter tail

let dump ppf t =
  Format.fprintf ppf "parser: %a\n" Merlin_parser.dump t.parser;
  Format.fprintf ppf "recovery: %a\n" dump_recovering t.recovering

let dump_recoverable ppf t =
  let t = match t.recovering with
    | Some _ -> t
    | None -> {t with recovering = Some (rollbacks t.parser)}
  in
  dump ppf t
