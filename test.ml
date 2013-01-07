let token_to_string =
  let open Parser in function
    | AMPERAMPER -> "AMPERAMPER"
    | AMPERSAND -> "AMPERSAND"
    | AND -> "AND"
    | AS -> "AS"
    | ASSERT -> "ASSERT"
    | BACKQUOTE -> "BACKQUOTE"
    | BANG -> "BANG"
    | BAR -> "BAR"
    | BARBAR -> "BARBAR"
    | BARRBRACKET -> "BARRBRACKET"
    | BEGIN -> "BEGIN"
    | CHAR c -> "CHAR(" ^ String.escaped (String.make 1 c) ^ ")"
    | CLASS -> "CLASS"
    | COLON -> "COLON"
    | COLONCOLON -> "COLONCOLON"
    | COLONEQUAL -> "COLONEQUAL"
    | COLONGREATER -> "COLONGREATER"
    | COMMA -> "COMMA"
    | CONSTRAINT -> "CONSTRAINT"
    | DO -> "DO"
    | DONE -> "DONE"
    | DOT -> "DOT"
    | DOTDOT -> "DOTDOT"
    | DOWNTO -> "DOWNTO"
    | ELSE -> "ELSE"
    | END -> "END"
    | EOF -> "EOF"
    | EQUAL -> "EQUAL"
    | EXCEPTION -> "EXCEPTION"
    | EXTERNAL -> "EXTERNAL"
    | FALSE -> "FALSE"
    | FLOAT s -> "FLOAT(" ^ String.escaped s ^ ")"
    | FOR -> "FOR"
    | FUN -> "FUN"
    | FUNCTION -> "FUNCTION"
    | FUNCTOR -> "FUNCTOR"
    | GREATER -> "GREATER"
    | GREATERRBRACE -> "GREATERRBRACE"
    | GREATERRBRACKET -> "GREATERRBRACKET"
    | IF -> "IF"
    | IN -> "IN"
    | INCLUDE -> "INCLUDE"
    | INFIXOP0 s -> "INFIXOP0(" ^ String.escaped s ^ ")"
    | INFIXOP1 s -> "INFIXOP1(" ^ String.escaped s ^ ")"
    | INFIXOP2 s -> "INFIXOP2(" ^ String.escaped s ^ ")"
    | INFIXOP3 s -> "INFIXOP3(" ^ String.escaped s ^ ")"
    | INFIXOP4 s -> "INFIXOP4(" ^ String.escaped s ^ ")"
    | INHERIT -> "INHERIT"
    | INITIALIZER -> "INITIALIZER"
    | INT s -> "INT(" ^ string_of_int s ^ ")"
    | INT32 s -> "INT32(" ^ Int32.to_string s ^ ")"
    | INT64 s -> "INT64(" ^ Int64.to_string s ^ ")"
    | LABEL s -> "LABEL(" ^ String.escaped s ^ ")"
    | LAZY -> "LAZY"
    | LBRACE -> "LBRACE"
    | LBRACELESS -> "LBRACELESS"
    | LBRACKET -> "LBRACKET"
    | LBRACKETBAR -> "LBRACKETBAR"
    | LBRACKETLESS -> "LBRACKETLESS"
    | LBRACKETGREATER -> "LBRACKETGREATER"
    | LESS -> "LESS"
    | LESSMINUS -> "LESSMINUS"
    | LET -> "LET"
    | LIDENT s -> "LIDENT(" ^ String.escaped s ^ ")"
    | LPAREN -> "LPAREN"
    | MATCH -> "MATCH"
    | METHOD -> "METHOD"
    | MINUS -> "MINUS"
    | MINUSDOT -> "MINUSDOT"
    | MINUSGREATER -> "MINUSGREATER"
    | MODULE -> "MODULE"
    | MUTABLE -> "MUTABLE"
    | NATIVEINT s -> "NATIVEINT(" ^ Nativeint.to_string s ^ ")"
    | NEW -> "NEW"
    | OBJECT -> "OBJECT"
    | OF -> "OF"
    | OPEN -> "OPEN"
    | OPTLABEL s -> "OPTLABEL(" ^ String.escaped s ^ ")"
    | OR -> "OR"
    | PLUS -> "PLUS"
    | PLUSDOT -> "PLUSDOT"
    | PREFIXOP s -> "PREFIXOP(" ^ String.escaped s ^ ")"
    | PRIVATE -> "PRIVATE"
    | QUESTION -> "QUESTION"
    | QUESTIONQUESTION -> "QUESTIONQUESTION"
    | QUOTE -> "QUOTE"
    | RBRACE -> "RBRACE"
    | RBRACKET -> "RBRACKET"
    | REC -> "REC"
    | RPAREN -> "RPAREN"
    | SEMI -> "SEMI"
    | SEMISEMI -> "SEMISEMI"
    | SHARP -> "SHARP"
    | SIG -> "SIG"
    | STAR -> "STAR"
    | STRING s -> "STRING(" ^ String.escaped s ^ ")"
    | STRUCT -> "STRUCT"
    | THEN -> "THEN"
    | TILDE -> "TILDE"
    | TO -> "TO"
    | TRUE -> "TRUE"
    | TRY -> "TRY"
    | TYPE -> "TYPE"
    | UIDENT s -> "UIDENT(" ^ String.escaped s ^ ")"
    | UNDERSCORE -> "UNDERSCORE"
    | VAL -> "VAL"
    | VIRTUAL -> "VIRTUAL"
    | WHEN -> "WHEN"
    | WHILE -> "WHILE"
    | WITH -> "WITH"
    | COMMENT (s,_) -> "COMMENT(" ^ String.escaped s ^ ")"

let wrap f lb =
  let t = (f lb) in
  print_endline (token_to_string t);
  t

let parse_with ?token parser lexer buf =
  let tokens = ref [] in
  let fake_token = ref token in
  let fake_position = ref None in
  let set_position buf start curr =
    Lexing.(buf.lex_start_p <- start; buf.lex_curr_p <- curr)
  in
  let wrap buf =
    let ret =
      match !fake_token with
        | Some (token,start,curr) ->
            fake_position := Some (Lexing.(buf.lex_start_p, buf.lex_curr_p));
            fake_token := None;
            set_position buf start curr;
            token
        | None ->
      match !fake_position with
        | Some (start,curr) ->
            fake_position := None;
            set_position buf start curr;
            lexer buf
        | None ->
            lexer buf
    in
    tokens := ret :: !tokens;
    ret
  in
  try
    let () = parser wrap buf in
    Parsing.clear_parser ();
    None, !tokens, Outline.Done
  with
    | Outline.Chunk (c,p) ->
        begin
          Parsing.clear_parser ();
          match !tokens with
          | t :: ts when p <> buf.Lexing.lex_curr_p ->
              prerr_endline "refill";
              Some t, ts, c
          | ts ->
              None, ts, c
        end
    | exn ->
        Parsing.clear_parser ();
        raise exn
  
let _ =
  let buf = Lexing.from_channel stdin in
  let lookahead, tokens, chunk =
    parse_with Parser.implementation Lexer.token buf
  in
  List.iter (fun tok -> print_string (token_to_string tok); print_char ' ') tokens
