let lexbuf = Lexing.from_channel stdin

let chunks = Ocamllex_parser.lexer_chunks Ocamllex_lexer.main lexbuf

let () =
  let split_pos {Lexing. pos_lnum; pos_cnum; pos_bol; _} =
    (pos_lnum, pos_cnum - pos_bol)
  in
  let split_loc {Location. loc_start; loc_end; _} =
    (split_pos loc_start, split_pos loc_end)
  in
  List.iter (function
      | Ocamllex_syntax.Action l ->
        let (sl, sc), (el, ec) = split_loc l in
        Printf.printf "Action %d:%d-%d:%d\n" sl sc el ec
      | Ocamllex_syntax.Rule {name; args; clauses} ->
        Printf.printf "Rule %s, %d clauses\n"
          (String.concat " "
             (List.map (fun x -> x.Location.txt) (name :: args)))
          (List.length clauses)
      | Ocamllex_syntax.And_rule {name; args; clauses} ->
        Printf.printf "And_rule %s, %d clauses\n"
          (String.concat " "
             (List.map (fun x -> x.Location.txt) (name :: args)))
          (List.length clauses)
      | Ocamllex_syntax.Let_regexp (name, _) ->
        Printf.printf "Let_regexp %s\n" name.Location.txt
      | Ocamllex_syntax.Refill_handler l ->
        let (sl, sc), (el, ec) = split_loc l in
        Printf.printf "Refill_handler %d:%d-%d:%d\n" sl sc el ec
    ) chunks
