module Utils = Outline_utils

module Raw =
struct
  type token = Chunk_parser.token
  type item = token History.loc  
  type sync = item History.sync
  type t = item History.t 

  let token_to_string =
    let open Chunk_parser in function
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
end
  
let parse_with history ~parser ~lexer buf =
  let origin = History.current_pos history in
  let history' = ref history in
  let chunk_content h =
    let open History in
    (* Drop end of history *)
    let end_of_chunk, _ = split h in
    let at_origin = seek_pos origin end_of_chunk in
    (* Drop beginning of history *)
    let _, chunk_content = split at_origin in
    History.nexts chunk_content
  in
  try
    let () = parser (History.wrap_lexer history' lexer) buf in
    (* Parsing.clear_parser (); *)
    let history = !history' in
    history, Outline_utils.Done, chunk_content history
  with
    | Outline_utils.Chunk (c,p) ->
        begin
          (* Parsing.clear_parser (); *)
          let history =
            if p <> buf.Lexing.lex_curr_p
            then (prerr_endline "refill";
                  match History.backward !history' with
                    | Some (_,h) -> h
                    | None -> !history')
            else !history'
          in
          history, c, chunk_content history
        end
    | Sys.Break ->
        begin
          (* Parsing.clear_parser (); *)
          let history = !history' in
          History.(seek_pos origin history),
          Outline_utils.Unterminated,
          []
        end
    (*| exn ->
        (* Parsing.clear_parser (); *)
        raise exn*)

module Chunked =
struct
  type item = Raw.sync * Outline_utils.kind * Raw.item list
  type sync = item History.sync
  type t = item History.t 
end

let parse_step history buf =
  let history', kind, tokens = parse_with history
    ~parser:Outline_parser.implementation
    ~lexer:Outline_lexer.token
    buf
  in
  history', (History.sync_point history', kind, tokens)

let parse (history,chunks) buf =
  match parse_step history buf with
    | history', (_, Outline_utils.Rollback, _) ->
        let chunks' = History.move (-1) history' in
        (*history', History.modify_current (fun (c,l) -> (c, l @ data)) chunks'*)
        failwith "TODO"
    | history', (_, Outline_utils.Unterminated, _) ->
        history', chunks
    | history', (_, chunk, data) ->
        history', History.insert (History.sync_point history',chunk,data) chunks
