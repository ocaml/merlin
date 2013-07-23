(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013  Frédéric Bour  <frederic.bour(_)lakaban.net>
                      Thomas Refis  <refis.thomas(_)gmail.com>
                      Simon Castellan  <simon.castellan(_)iuwt.fr>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

type token = Chunk_parser.token

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
    | JSNEW -> "JSNEW"
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
    | QUOTE -> "QUOTE"
    | RBRACE -> "RBRACE"
    | RBRACKET -> "RBRACKET"
    | REC -> "REC"
    | NONREC -> "NONREC"
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
    | LET_LWT -> "LET_LWT"
    | TRY_LWT -> "TRY_LWT"
    | FINALLY_LWT -> "FINALLY_LWT"
    | MATCH_LWT -> "MATCH_LWT"
    | FOR_LWT -> "FOR_LWT"
    | WHILE_LWT -> "WHILE_LWT"
    | P4_QUOTATION -> "P4_QUOTATION"
    | OUNIT_TEST -> "TEST"
    | OUNIT_TEST_UNIT -> "TEST_UNIT"
    | OUNIT_TEST_MODULE -> "TEST_MODULE"
    | OUNIT_BENCH -> "BENCH"
    | OUNIT_BENCH_FUN -> "BENCH_FUN"
    | OUNIT_BENCH_INDEXED -> "BENCH_INDEXED"
    | OUNIT_BENCH_MODULE -> "BENCH_MODULE"

let dump_lexer ?who f a =
  let t = f a in
  (*Printf.eprintf "%s:%s, %!" 
   (match who with Some w -> w | None -> "")
   (token_to_string t);*)
  t

let rec re_sync lexer buf =
  let open Chunk_parser in
  match lexer buf with
    | SEMISEMI | EOF -> 0
    | INCLUDE | OPEN | LET | TYPE | EXCEPTION -> 1
    | MODULE | END -> 2
    | _ -> re_sync lexer buf

