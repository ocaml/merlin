let lexbuf = Lexing.from_channel stdin

let lex_only = true || Array.length Sys.argv > 1 && Sys.argv.(1) = "-lex"

let string_of_token : Menhir_parser.token -> string = function
  | ACTION _           -> "ACTION _"
  | ATTRIBUTE _        -> "ATTRIBUTE _"
  | BAR                -> "BAR"
  | COLON              -> "COLON"
  | COLONEQUAL         -> "COLONEQUAL"
  | COMMA              -> "COMMA"
  | EOF                -> "EOF"
  | EQUAL              -> "EQUAL"
  | EQUALEQUAL         -> "EQUALEQUAL"
  | GRAMMARATTRIBUTE _ -> "GRAMMARATTRIBUTE _"
  | HEADER _           -> "HEADER _"
  | INLINE             -> "INLINE"
  | LEFT               -> "LEFT"
  | LET                -> "LET"
  | LID _              -> "LID _"
  | LPAREN             -> "LPAREN"
  | NONASSOC           -> "NONASSOC"
  | OCAMLTYPE _        -> "OCAMLTYPE _"
  | ON_ERROR_REDUCE    -> "ON_ERROR_REDUCE"
  | PARAMETER          -> "PARAMETER"
  | PERCENTATTRIBUTE   -> "PERCENTATTRIBUTE"
  | PERCENTPERCENT _   -> "PERCENTPERCENT _"
  | PLUS               -> "PLUS"
  | PREC               -> "PREC"
  | PUBLIC             -> "PUBLIC"
  | QID _              -> "QID _"
  | QUESTION           -> "QUESTION"
  | RIGHT              -> "RIGHT"
  | RPAREN             -> "RPAREN"
  | SEMI               -> "SEMI"
  | STAR               -> "STAR"
  | START              -> "START"
  | TILDE              -> "TILDE"
  | TOKEN              -> "TOKEN"
  | TYPE               -> "TYPE"
  | UID _              -> "UID _"
  | UNDERSCORE         -> "UNDERSCORE"

let lex () =
  let rec loop () =
    let token = Menhir_lexer.main lexbuf in
    print_endline (string_of_token token);
    match token with
    | EOF -> ()
    | _ -> loop ()
  in
  loop ()

let parse () =
  ()
  (*let chunks = Menhir_parser.lexer_chunks Menhir_lexer.main lexbuf in
  let split_pos {Lexing. pos_lnum; pos_cnum; pos_bol; _} =
    (pos_lnum, pos_cnum - pos_bol)
  in
  let split_loc {Location. loc_start; loc_end; _} =
    (split_pos loc_start, split_pos loc_end)
  in
  List.iter (function
      | Menhir_syntax.Action l ->
        let (sl, sc), (el, ec) = split_loc l in
        Printf.printf "Action %d:%d-%d:%d\n" sl sc el ec
      | Menhir_syntax.Rule {name; args; clauses} ->
        Printf.printf "Rule %s, %d clauses\n"
          (String.concat " "
             (List.map (fun x -> x.Location.txt) (name :: args)))
          (List.length clauses)
      | Menhir_syntax.And_rule {name; args; clauses} ->
        Printf.printf "And_rule %s, %d clauses\n"
          (String.concat " "
             (List.map (fun x -> x.Location.txt) (name :: args)))
          (List.length clauses)
      | Menhir_syntax.Let_regexp (name, _) ->
        Printf.printf "Let_regexp %s\n" name.Location.txt
      | Menhir_syntax.Refill_handler l ->
        let (sl, sc), (el, ec) = split_loc l in
        Printf.printf "Refill_handler %d:%d-%d:%d\n" sl sc el ec
      | Menhir_syntax.Syntax_error l ->
        let (sl, sc), (el, ec) = split_loc l in
        Printf.printf "Syntax_error %d:%d-%d:%d\n" sl sc el ec
    ) chunks*)

let () = if lex_only then lex () else parse ()
