open Std
open Raw_parser

let in_struct parser =
  match Merlin_parser.Frame.stack parser with
  | None -> false
  | Some frame ->
    match Merlin_parser.Frame.value frame with
    | Nonterminal ( NT'with_recover_structure_item_ _
                      | NT'with_recover_seq_expr_ _
                      | NT'structure_sep _ ) -> true
    | _ -> false

let rec drop_items = function
  | parser :: parsers when in_struct parser -> drop_items parsers
  | parsers -> parsers

let rollbacks parser =
  let flag = ref true in
  let rec aux ?location = function
    | [] -> []
    | parser :: parsers ->
      let loc' = Merlin_parser.location parser in
      match Merlin_parser.recover ?location flag parser with
      | None -> aux ~location:loc' parsers
      | Some parser ->
        let parsers =
          if in_struct parser.Location.txt then
            drop_items parsers
          else
            parsers
        in
        parser :: aux ~location:loc' parsers
  in
  let stacks = List.unfold Merlin_parser.pop parser in
  let recoverable = aux stacks in
  let last_pos = ref (-1) in
  let recoverable =
    List.filter_map recoverable
    ~f:(fun t ->
        let start = snd (Lexing.split_pos t.Location.loc.Location.loc_start) in
        if start = !last_pos
        then None
        else (last_pos := start; Some t)
       )
  in
  Zipper.of_list recoverable, flag

type t = {
  errors: exn list;
  parser: Merlin_parser.t;
  recovering: (Merlin_parser.t Location.loc zipper * bool ref) option;
}

let parser t = t.parser
let exns t = t.errors

let fresh parser = { errors = []; parser; recovering = None }

let feed_normal (_,tok,_ as input) parser =
  Logger.debugf `internal
    (fun ppf tok -> Format.fprintf ppf "normal parser: received %s"
        (Merlin_parser.Values.Token.to_string tok))
    tok;
  match Merlin_parser.feed input parser with
  | `Accept _ ->
    Logger.debug `internal "parser accepted";
    Some parser
  | `Reject _ ->
    Logger.debug `internal "parser rejected";
    None
  | `Step parser ->
    Some parser

let closing_token = function
  | END -> true
  | _ -> false

let feed_recover original (s,tok,e as input) zipper =
  let get_col x = snd (Lexing.split_pos x) in
  let col = get_col s in
  (* Find appropriate recovering position *)
  let to_the_right =
    Zipper.seek_backward
      (fun cell -> col > get_col Location.(cell.loc.loc_start))
  and to_the_left =
    Zipper.seek_forward
      (fun cell -> get_col Location.(cell.loc.loc_start) > col)
  in
  let zipper = to_the_right zipper in
  let zipper = to_the_left zipper in
  (* Closing tokens are applied one step behind *)
  let zipper =
    Zipper.shift (if closing_token tok then -1 else 0)
      zipper
  in
  match zipper with
  | Zipper ([], _, []) -> Either.R original
  | Zipper (_, _, cell :: _) | Zipper (cell :: _, _, _) ->
    let candidate = cell.Location.txt in
    match Merlin_parser.feed input candidate with
    | `Accept _ | `Reject _ ->
      Either.L zipper
    | `Step parser ->
      let diff = Merlin_reconstruct.diff ~stack:original ~wrt:parser in
      match Merlin_parser.reconstruct
              (Merlin_reconstruct.Partial_stack diff)
              candidate
      with
      | None -> assert false
      | Some parser ->
        match Merlin_parser.feed input parser with
        | `Accept _ | `Reject _ -> assert false
        | `Step parser -> Either.R parser

let fold warnings token t =
  match token with
  | Merlin_lexer.Error _ -> t
  | Merlin_lexer.Valid (s,tok,e) ->
    Logger.debugf `internal
      (fun ppf tok -> Format.fprintf ppf "received %s"
          (Merlin_parser.Values.Token.to_string tok))
      tok;
    Logger.debugf `internal Merlin_parser.dump t.parser;
    warnings := [];
    let pop w = let r = !warnings in w := []; r in
    let recover_from t (recovery, flag) =
      match feed_recover t.parser (s,tok,e) recovery with
      | Either.L recovery ->
        {t with recovering = Some (recovery, flag)}
      | Either.R parser ->
        {t with parser; recovering = None}
    in
    match t.recovering with
    | Some recovery -> recover_from t recovery
    | None ->
      begin match feed_normal (s,tok,e) t.parser with
        | None ->
          let recovery = rollbacks t.parser in
          let error =
            Error_classifier.from (Merlin_parser.to_step t.parser) (s,tok,e)
          in
          recover_from
            {t with errors = error :: (pop warnings) @ t.errors; }
            recovery
        | Some parser ->
          {t with errors = (pop warnings) @ t.errors; parser }
      end

let fold token t =
  let warnings = ref [] in
  Either.get (Parsing_aux.catch_warnings warnings
                (fun () -> fold warnings token t))

let dump_snapshot ppf s =
  Format.fprintf ppf "- position: %a\n  parser: %a\n"
    Location.print s.Location.loc
    Merlin_parser.dump s.Location.txt

let dump_recovering ppf = function
  | None -> Format.fprintf ppf "clean"
  | Some (Zipper (head, _, tail), _) ->
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

