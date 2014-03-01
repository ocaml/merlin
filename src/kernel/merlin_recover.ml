open Std
open Raw_parser

let rollbacks parser =
  let stacks = List.Lazy.unfold Merlin_parser.pop parser in
  let recoverable = List.Lazy.filter_map Merlin_parser.recover stacks in
  recoverable

type t = {
  errors: exn list;
  parser: Merlin_parser.t;
  recovering: (Merlin_parser.t Location.loc list *
               Merlin_parser.t Location.loc List.Lazy.t) option;
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

let feed_recover (s,tok,e as input) (hd,tl) =
  let get_col x = snd (Lexing.split_pos x) in
  let col = get_col s in
  (* Find appropriate recovering position *)
  let rec to_the_right hd tl =
    match hd with
    | cell :: hd' when col > get_col cell.Location.loc.Location.loc_start ->
      to_the_right hd' (List.Lazy.Cons (cell, Lazy.from_val tl))
    | _ -> hd, tl
  in
  let rec to_the_left hd tl =
    match tl with
    | List.Lazy.Cons (cell, lazy tl)
      when get_col cell.Location.loc.Location.loc_start > col ->
      to_the_left (cell :: hd) tl
    | _ -> hd, tl
  in
  let hd, tl = to_the_right hd tl in
  let hd, tl = to_the_left hd tl in
  match hd, tl with
  | [], List.Lazy.Nil -> assert false
  | _, List.Lazy.Cons (cell, _) | (cell :: _), _ ->
    match Merlin_parser.feed input cell.Location.txt with
    | `Accept _ | `Reject _ ->
      Either.L (hd,tl)
    | `Step parser ->
      Either.R parser

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
    let recover_from t recovery =
      match feed_recover (s,tok,e) recovery with
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
          let recovery = ([], rollbacks t.parser) in
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
