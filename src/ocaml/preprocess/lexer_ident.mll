(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* The lexer definition *)

{
open Std
open Lexing
open Parser_raw

(* Update the current location with file name and line number. *)

let update_loc lexbuf file line absolute chars =
  let pos = lexbuf.lex_curr_p in
  let new_file = match file with
                 | None -> pos.pos_fname
                 | Some s -> s
  in
  lexbuf.lex_curr_p <- { pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }

(* This lexer is only used to reconstruct the identifier around a position, but
   it still needs to know whether the position is inside a string literal:
   otherwise the contents of string literals gets lexed as regular code, and
   e.g. asking for the documentation of the [/] in ["/usr/local/home"] answers
   with the documentation of the integer division.

   String literals are therefore skipped, except for [%{...}] interpolations
   (as used by [ppx_string] and friends), whose contents are lexed as regular
   code so that identifier reconstruction keeps working inside them. *)
type string_kind =
  | Double_quoted             (* "..." *)
  | Quoted of string          (* {tag|...|tag}, argument is the tag *)

type state =
  { mutable in_interpolation : string_kind option
        (* [Some kind] when the lexer is inside a [%{...}] interpolation of a
           string literal of kind [kind]. *)
  }

let make_state () = { in_interpolation = None }
}

let newline = ('\013'* '\010')
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '_']
let uppercase = ['A'-'Z']
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']
let lowercase_latin1 = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase_latin1 = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar_latin1 =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let symbolcharnopercent =
  ['!' '$' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
let int_literal =
  decimal_literal | hex_literal | oct_literal | bin_literal
let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?
let dotsymbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '/' ':' '=' '>' '?' '@' '^' '|']
let kwdopchar =
  ['$' '&' '*' '+' '-' '/' '<' '=' '>' '@' '^' '|']

rule token state = parse
  | "_" { EOL }
  | newline
      { update_loc lexbuf None 1 false 0;
        token state lexbuf }
  | blank +
      { token state lexbuf }
  | "~" (lowercase identchar *) as label ':'
      { LABEL label }
  | "~" (lowercase_latin1 identchar_latin1 *) as label ':'
      { LABEL label }
  | "?"
      { QUESTION }
  | "?" (lowercase identchar *) as label ':'
      { OPTLABEL label }
  | "?" (lowercase_latin1 identchar_latin1 *) as label ':'
      { OPTLABEL label }
  | ("let" kwdopchar dotsymbolchar *) as op { LETOP op }
  | ("and" kwdopchar dotsymbolchar *) as op { ANDOP op }
  | (lowercase identchar *) as ident
    { LIDENT ident }
  | (lowercase_latin1 identchar_latin1 *) as ident
    { LIDENT ident }
  | (uppercase identchar *) as ident
    { UIDENT ident }
  | "`"  { BACKQUOTE }
  | "'"  { QUOTE }
  | "("  { LPAREN }
  | ")"  { RPAREN }
  | "."  { DOT }
  | "!" symbolchar +
            { PREFIXOP(Lexing.lexeme lexbuf) }
  | ['~' '?'] symbolchar +
            { PREFIXOP(Lexing.lexeme lexbuf) }
  | ['=' '<' '|' '&' '$' '>'] symbolchar *
            { INFIXOP0(Lexing.lexeme lexbuf) }
  | ['@' '^'] symbolchar *
            { INFIXOP1(Lexing.lexeme lexbuf) }
  | ['+' '-'] symbolchar *
            { INFIXOP2(Lexing.lexeme lexbuf) }
  | "**" symbolchar *
            { INFIXOP4(Lexing.lexeme lexbuf) }
  | '%'     { PERCENT }
  | ['*' '/' '%'] symbolchar *
            { INFIXOP3(Lexing.lexeme lexbuf) }
  | '#' (symbolchar | '#') +
            { let s = Lexing.lexeme lexbuf in
              HASHOP s }
  | "\""
            { skip_double_quoted_string state lexbuf }
  | "{" (lowercase* as tag) "|"
            { skip_quoted_string state tag lexbuf }
  | "}"
            { match state.in_interpolation with
              | Some kind ->
                state.in_interpolation <- None;
                (match kind with
                 | Double_quoted -> skip_double_quoted_string state lexbuf
                 | Quoted tag -> skip_quoted_string state tag lexbuf)
              | None -> EOL }
  | eof { EOF }
  | "'" newline "'"
      { update_loc lexbuf None 1 false 1;
        EOL }
  | "'\\" newline
      { update_loc lexbuf None 1 false 0;
        EOL }
  | int_literal
  | float_literal
  | int_literal "l"
  | int_literal "L"
  | int_literal "n"
  | ".<"
  | ">."
  | ".~"
  | "~"
  | "'" [^ '\\' '\'' '\010' '\013'] "'"
  | "'\\" ['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] "'"
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
  | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
  | "'\\" _
  | "(*"
  | "(*)"
  | "*)"
  | "#"
  | "&"
  | "&&"
  | "*"
  | ","
  | "->"
  | ".."
  | ":"
  | "::"
  | ":="
  | ":>"
  | ";"
  | ";;"
  | "<"
  | "<-"
  | "="
  | "["
  | "[|"
  | "[<"
  | "[>"
  | "]"
  | "{"
  | "{<"
  | "|"
  | "||"
  | "|]"
  | ">"
  | ">]"
  | ">}"
  | "[@"
  | "[%"
  | "[%%"
  | "[@@"
  | "[@@@"
  | "!"

  | "!="
  | "+"
  | "+."
  | "+="
  | "-"
  | "-."
    { EOL }
  | _ { EOL }

(* Skip the contents of a string literal, stopping either at the closing
   delimiter (returning [EOL], so that the whole literal behaves like a token
   separator) or at the start of a [%{...}] interpolation (recording in [state]
   that the tokens that follow are inside an interpolation, so that the
   matching [}] in [token] resumes skipping the enclosing string). *)
and skip_double_quoted_string state = parse
  | "%{"
      { state.in_interpolation <- Some Double_quoted;
        EOL }
  | '\\' newline
      { update_loc lexbuf None 1 false 0;
        skip_double_quoted_string state lexbuf }
  | '\\' _
      { skip_double_quoted_string state lexbuf }
  | "\"" { EOL }
  | newline
      { update_loc lexbuf None 1 false 0;
        skip_double_quoted_string state lexbuf }
  | eof { EOF }
  | _ { skip_double_quoted_string state lexbuf }

(* Same as [skip_double_quoted_string], but for [{tag|...|tag}] literals, which
   do not have escape sequences. *)
and skip_quoted_string state tag = parse
  | "%{"
      { state.in_interpolation <- Some (Quoted tag);
        EOL }
  | "|" (lowercase* as tag') "}"
      { if String.equal tag tag' then EOL
        else skip_quoted_string state tag lexbuf }
  | newline
      { update_loc lexbuf None 1 false 0;
        skip_quoted_string state tag lexbuf }
  | eof { EOF }
  | _ { skip_quoted_string state tag lexbuf }

