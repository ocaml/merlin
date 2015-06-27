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
open Misc
open Lexing
open Raw_parser

type keywords = (string, Raw_parser.token) Hashtbl.t

type error =
  | Illegal_character of char
  | Illegal_escape of string
  | Unterminated_comment of Location.t
  | Unterminated_string
  | Unterminated_string_in_comment of Location.t * Location.t
  | Keyword_as_label of string
  | Literal_overflow of string

exception Error of error * Location.t

(* Monad in which the lexer evaluates *)
type 'a result =
  | Return of 'a
  | Refill of (unit -> 'a result)
  | Fail of error * Location.t

let return a = Return a

let fail e l = Fail (e,l)

let rec (>>=) (m : 'a result) (f : 'a -> 'b result) : 'b result =
  match m with
  | Return a -> f a
  | Refill u ->
    Refill (fun () -> u () >>= f)
  | Fail _ as e -> e

type preprocessor = (Lexing.lexbuf -> Raw_parser.token) -> Lexing.lexbuf -> Raw_parser.token

type state = {
  keywords: keywords;
  buffer: Buffer.t;
  mutable string_start_loc: Location.t;
  mutable comment_start_loc: Location.t list;
  mutable preprocessor: preprocessor option;
}

let make ?preprocessor keywords = {
  keywords;
  buffer = Buffer.create 17;
  string_start_loc = Location.none;
  comment_start_loc = [];
  preprocessor;
}

let lABEL m = m >>= fun v -> return (LABEL v)
let oPTLABEL m = m >>= fun v -> return (OPTLABEL v)

let rec catch m f = match m with
  | Fail (e,l) -> f e l
  | Refill next -> Refill (fun () -> catch (next ()) f)
  | Return _ -> m

(* The table of keywords *)

let keyword_table : keywords =
  create_hashtable 149 [
    "and", AND;
    "as", AS;
    "assert", ASSERT;
    "begin", BEGIN;
    "class", CLASS;
    "constraint", CONSTRAINT;
    "do", DO;
    "done", DONE;
    "downto", DOWNTO;
    "else", ELSE;
    "end", END;
    "exception", EXCEPTION;
    "external", EXTERNAL;
    "false", FALSE;
    "for", FOR;
    "fun", FUN;
    "function", FUNCTION;
    "functor", FUNCTOR;
    "if", IF;
    "in", IN;
    "include", INCLUDE;
    "inherit", INHERIT;
    "initializer", INITIALIZER;
    "lazy", LAZY;
    "let", LET;
    "match", MATCH;
    "method", METHOD;
    "module", MODULE;
    "mutable", MUTABLE;
    "new", NEW;
    "object", OBJECT;
    "of", OF;
    "open", OPEN;
    "or", OR;
(*  "parser", PARSER; *)
    "private", PRIVATE;
    "rec", REC;
    "sig", SIG;
    "struct", STRUCT;
    "then", THEN;
    "to", TO;
    "true", TRUE;
    "try", TRY;
    "type", TYPE;
    "val", VAL;
    "virtual", VIRTUAL;
    "when", WHEN;
    "while", WHILE;
    "with", WITH;

    "mod", INFIXOP3("mod");
    "land", INFIXOP3("land");
    "lor", INFIXOP3("lor");
    "lxor", INFIXOP3("lxor");
    "lsl", INFIXOP4("lsl");
    "lsr", INFIXOP4("lsr");
    "asr", INFIXOP4("asr");
]

let keywords l = create_hashtable 11 l

(* To store the position of the beginning of a string and comment *)
let in_comment state = state.comment_start_loc <> []
let in_string state = state.string_start_loc != Location.none

(* To translate escape sequences *)

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let char_for_decimal_code state lexbuf i =
  let c = 100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
           10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48) in
  if (c < 0 || c > 255) then
    if in_comment state
    then return 'x'
    else fail (Illegal_escape (Lexing.lexeme lexbuf))
               (Location.curr lexbuf)
  else return (Char.chr c)

let char_for_hexadecimal_code lexbuf i =
  let d1 = Char.code (Lexing.lexeme_char lexbuf i) in
  let val1 = if d1 >= 97 then d1 - 87
             else if d1 >= 65 then d1 - 55
             else d1 - 48
  in
  let d2 = Char.code (Lexing.lexeme_char lexbuf (i+1)) in
  let val2 = if d2 >= 97 then d2 - 87
             else if d2 >= 65 then d2 - 55
             else d2 - 48
  in
  Char.chr (val1 * 16 + val2)

(* To convert integer literals, allowing max_int + 1 (PR#4210) *)

let cvt_int_literal s =
  - int_of_string ("-" ^ s)
let cvt_int32_literal s =
  Int32.neg (Int32.of_string ("-" ^ String.sub s 0 (String.length s - 1)))
let cvt_int64_literal s =
  Int64.neg (Int64.of_string ("-" ^ String.sub s 0 (String.length s - 1)))
let cvt_nativeint_literal s =
  Nativeint.neg (Nativeint.of_string ("-" ^ String.sub s 0
                                                       (String.length s - 1)))

let keyword_or state s default =
  try Hashtbl.find state.keywords s
  with Not_found -> try Hashtbl.find keyword_table s
  with Not_found -> default

(* Remove underscores from float literals *)

let remove_underscores s =
  let l = String.length s in
  let rec remove src dst =
    if src >= l then
      if dst >= l then s else String.sub s 0 dst
    else
      match s.[src] with
        '_' -> remove (src + 1) dst
      |  c  -> s.[dst] <- c; remove (src + 1) (dst + 1)
  in remove 0 0

(* recover the name from a LABEL or OPTLABEL token *)

let get_label_name lexbuf =
  let s = Lexing.lexeme lexbuf in
  let name = String.sub s 1 (String.length s - 2) in
  if Hashtbl.mem keyword_table name then
    fail (Keyword_as_label name) (Location.curr lexbuf)
  else
    return name
;;

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
;;

(* Warn about Latin-1 characters used in idents *)

let warn_latin1 lexbuf =
  Location.prerr_warning (Location.curr lexbuf)
    (Warnings.Deprecated "ISO-Latin1 characters in identifiers")
;;

(* Error report *)

open Format

let report_error ppf = function
  | Illegal_character c ->
      fprintf ppf "Illegal character (%s)" (Char.escaped c)
  | Illegal_escape s ->
      fprintf ppf "Illegal backslash escape in string or character (%s)" s
  | Unterminated_comment _ ->
      fprintf ppf "Comment not terminated"
  | Unterminated_string ->
      fprintf ppf "String literal not terminated"
  | Unterminated_string_in_comment (_, loc) ->
      fprintf ppf "This comment contains an unterminated string literal@.\
                   %aString literal begins here"
              Location.print_error loc
  | Keyword_as_label kwd ->
      fprintf ppf "`%s' is a keyword, it cannot be used as label name" kwd
  | Literal_overflow ty ->
      fprintf ppf "Integer literal exceeds the range of representable \
                   integers of type %s" ty

let () =
  Location.register_error_of_exn
    (function
      | Error (err, loc) ->
        Some (Location.error_of_printer loc report_error err)
      | _ ->
        None
    )

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
let symbolcharnodot =
  ['!' '$' '%' '&' '*' '+' '-'     '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
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

refill {fun k lexbuf -> Refill (fun () -> k lexbuf)}

rule token state = parse
  | "\\" newline {
      match state.preprocessor with
      | None ->
        fail (Illegal_character (Lexing.lexeme_char lexbuf 0))
              (Location.curr lexbuf)
      | Some _ ->
        update_loc lexbuf None 1 false 0;
        token state lexbuf }
  | newline
      { update_loc lexbuf None 1 false 0;
        match state.preprocessor with
        | None -> token state lexbuf
        | Some _ -> return EOL
      }
  | blank +
      { token state lexbuf }
  | ".<"
      { return DOTLESS }
  | ">."
      { return (keyword_or state (Lexing.lexeme lexbuf) (INFIXOP0 ">.")) }
  | ".~"
      { return (keyword_or state (Lexing.lexeme lexbuf) DOTTILDE) }
  | "_"
      { return UNDERSCORE }
  | "~"
      { return TILDE }
  | "~" lowercase identchar * ':'
      { lABEL (get_label_name lexbuf) }
  | "~" lowercase_latin1 identchar_latin1 * ':'
      { warn_latin1 lexbuf; lABEL (get_label_name lexbuf) }
  | "?"
      { return QUESTION }
  | "?" lowercase identchar * ':'
      { oPTLABEL (get_label_name lexbuf) }
  | "?" lowercase_latin1 identchar_latin1 * ':'
      { warn_latin1 lexbuf; oPTLABEL (get_label_name lexbuf) }
  | lowercase identchar *
    { let s = Lexing.lexeme lexbuf in
      return (try Hashtbl.find state.keywords s
              with Not_found ->
              try Hashtbl.find keyword_table s
              with Not_found ->
                LIDENT s) }
  | lowercase_latin1 identchar_latin1 *
      { warn_latin1 lexbuf; return (LIDENT (Lexing.lexeme lexbuf)) }
  | uppercase identchar *
    { (* Capitalized keywords for OUnit *)
      let s = Lexing.lexeme lexbuf in
      return (try Hashtbl.find state.keywords s
              with Not_found ->
              try Hashtbl.find keyword_table s
              with Not_found ->
                UIDENT s) }
  | int_literal
      { try
          return (INT (cvt_int_literal (Lexing.lexeme lexbuf)))
        with Failure _ ->
          fail (Literal_overflow "int") (Location.curr lexbuf)
      }
  | float_literal
      { return (FLOAT (remove_underscores(Lexing.lexeme lexbuf))) }
  | int_literal "l"
      { try
          return (INT32 (cvt_int32_literal (Lexing.lexeme lexbuf)))
        with Failure _ ->
          fail (Literal_overflow "int32") (Location.curr lexbuf) }
  | int_literal "L"
      { try
          return (INT64 (cvt_int64_literal (Lexing.lexeme lexbuf)))
        with Failure _ ->
          fail (Literal_overflow "int64") (Location.curr lexbuf) }
  | int_literal "n"
      { try
          return (NATIVEINT (cvt_nativeint_literal (Lexing.lexeme lexbuf)))
        with Failure _ ->
          fail (Literal_overflow "nativeint") (Location.curr lexbuf) }
  | "\""
      { Buffer.reset state.buffer;
        state.string_start_loc <- Location.curr lexbuf;
        string state lexbuf >>= fun () ->
        lexbuf.lex_start_p <- state.string_start_loc.Location.loc_start;
        state.string_start_loc <- Location.none;
        return (STRING (Buffer.contents state.buffer, None))
      }
  | "{" lowercase* "|"
      { Buffer.reset state.buffer;
        let delim = Lexing.lexeme lexbuf in
        let delim = String.sub delim 1 (String.length delim - 2) in
        state.string_start_loc <- Location.curr lexbuf;
        quoted_string state delim lexbuf >>= fun () ->
        lexbuf.lex_start_p <- state.string_start_loc.Location.loc_start;
        state.string_start_loc <- Location.none;
        return (STRING (Buffer.contents state.buffer, Some delim)) }
  | "'" newline "'"
    { update_loc lexbuf None 1 false 1;
      return (CHAR (Lexing.lexeme_char lexbuf 1)) }
  | "'" [^ '\\' '\'' '\010' '\013'] "'"
    { return (CHAR (Lexing.lexeme_char lexbuf 1)) }
  | "'\\" ['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] "'"
    { return (CHAR (char_for_backslash (Lexing.lexeme_char lexbuf 2))) }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
    { char_for_decimal_code state lexbuf 2 >>= fun c -> return (CHAR c) }
  | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
    { return (CHAR (char_for_hexadecimal_code lexbuf 3)) }
  | "'\\" _
      { let l = Lexing.lexeme lexbuf in
        let esc = String.sub l 1 (String.length l - 1) in
        fail (Illegal_escape esc) (Location.curr lexbuf)
      }
  | "(*"
      { let start_loc = Location.curr lexbuf in
        state.comment_start_loc <- [start_loc];
        Buffer.reset state.buffer;
        comment state lexbuf >>= fun end_loc ->
        let s = Buffer.contents state.buffer in
        Buffer.reset state.buffer;
        return (COMMENT (s, { start_loc with
                              Location.loc_end = end_loc.Location.loc_end }))
      }
  | "(*)"
      { let loc = Location.curr lexbuf in
        Location.prerr_warning loc Warnings.Comment_start;
        state.comment_start_loc <- [loc];
        Buffer.reset state.buffer;
        comment state lexbuf >>= fun end_loc ->
        let s = Buffer.contents state.buffer in
        Buffer.reset state.buffer;
        return (COMMENT (s, { loc with Location.loc_end = end_loc.Location.loc_end }))
      }
  | "*)"
      { let loc = Location.curr lexbuf in
        Location.prerr_warning loc Warnings.Comment_not_end;
        lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - 1;
        let curpos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- { curpos with pos_cnum = curpos.pos_cnum - 1 };
        return STAR
      }
  | "#" [' ' '\t']* (['0'-'9']+ as num) [' ' '\t']*
        ("\"" ([^ '\010' '\013' '"' ] * as name) "\"")?
        [^ '\010' '\013'] * newline
      { update_loc lexbuf name (int_of_string num) true 0;
        token state lexbuf
      }
  | "#"  { return SHARP }
  | "&"  { return AMPERSAND }
  | "&&" { return AMPERAMPER }
  | "`"  { return BACKQUOTE }
  | "'"  { return QUOTE }
  | "("  { return LPAREN }
  | ")"  { return RPAREN }
  | "*"  { return STAR }
  | ","  { return COMMA }
  | "->" { return MINUSGREATER }
  | "."  { return DOT }
  | ".." { return DOTDOT }
  | ":"  { return COLON }
  | "::" { return COLONCOLON }
  | ":=" { return COLONEQUAL }
  | ":>" { return COLONGREATER }
  | ";"  { return SEMI }
  | ";;" { return SEMISEMI }
  | "<"  { return LESS }
  | "<-" { return LESSMINUS }
  | "="  { return EQUAL }
  | "["  { return LBRACKET }
  | "[|" { return LBRACKETBAR }
  | "[<" { return LBRACKETLESS }
  | "[>" { return LBRACKETGREATER }
  | "]"  { return RBRACKET }
  | "{"  { return LBRACE }
  | "{<" { return LBRACELESS }
  | "|"  { return BAR }
  | "||" { return BARBAR }
  | "|]" { return BARRBRACKET }
  | ">"  { return GREATER }
  | ">]" { return GREATERRBRACKET }
  | "}"  { return RBRACE }
  | ">}" { return GREATERRBRACE }
  | "[@" { return LBRACKETAT }
  | "[%" { return LBRACKETPERCENT }
  | "[%%" { return LBRACKETPERCENTPERCENT }
  | "[@@" { return LBRACKETATAT }
  | "[@@@" { return LBRACKETATATAT }
  (* Custom-printf is implemented by generating a custom BANG token *)
  | "!"  { return (try Hashtbl.find state.keywords "!"
                   with Not_found -> BANG) }
  | "!=" { return (INFIXOP0 "!=") }
  | "+"  { return PLUS }
  | "+." { return PLUSDOT }
  | "+=" { return PLUSEQ }
  | "-"  { return MINUS }
  | "-." { return MINUSDOT }

  | "!" symbolchar +
            { return (PREFIXOP(Lexing.lexeme lexbuf)) }
  | ['~' '?'] symbolchar +
            { return (PREFIXOP(Lexing.lexeme lexbuf)) }
  | ['=' '<' '|' '&' '$'] symbolchar *
            { return (INFIXOP0(Lexing.lexeme lexbuf)) }
  | ['>'] symbolcharnodot symbolchar *
            { return (INFIXOP0(Lexing.lexeme lexbuf)) }
  | ['@' '^'] symbolchar *
            { return (INFIXOP1(Lexing.lexeme lexbuf)) }
  | ['+' '-'] symbolchar *
            { return (INFIXOP2(Lexing.lexeme lexbuf)) }
  | "**" symbolchar *
            { return (INFIXOP4(Lexing.lexeme lexbuf)) }
  | '%'     { return PERCENT }
  | ['*' '/' '%'] symbolchar *
            { return (INFIXOP3(Lexing.lexeme lexbuf)) }
  | '#' symbolchar (symbolchar | '#') *
            { return (SHARPOP(Lexing.lexeme lexbuf)) }
  | "let" symbolchar *
            { return (LETOP(Lexing.lexeme lexbuf)) }
  | eof { return EOF }

  | "<:" identchar* ("@" identchar*)? "<"
  | "<@" identchar* "<"
  | "<<" identchar
    { let start = lexbuf.lex_start_p in
      p4_quotation lexbuf >>= fun () ->
      lexbuf.lex_start_p <- start;
      return P4_QUOTATION
    }

  | _
      { fail (Illegal_character (Lexing.lexeme_char lexbuf 0))
              (Location.curr lexbuf)
      }

and comment state = parse
    "(*"
      { state.comment_start_loc <- (Location.curr lexbuf) :: state.comment_start_loc;
      Buffer.add_string state.buffer (Lexing.lexeme lexbuf);
      comment state lexbuf
    }
  | "*)"
      { match state.comment_start_loc with
        | [] -> assert false
        | [_] -> state.comment_start_loc <- []; return (Location.curr lexbuf)
        | _ :: l -> state.comment_start_loc <- l;
                  Buffer.add_string state.buffer (Lexing.lexeme lexbuf);
                  comment state lexbuf
       }
  | "\""
      {
        state.string_start_loc <- Location.curr lexbuf;
        Buffer.add_char state.buffer '"';
        (catch (string state lexbuf) (fun e l -> match e with
             | Unterminated_string ->
               begin match state.comment_start_loc with
                 | [] -> assert false
                 | loc :: _ ->
                   let start = List.hd (List.rev state.comment_start_loc) in
                   state.comment_start_loc <- [];
                   fail (Unterminated_string_in_comment (start, l)) loc
               end
             | e -> fail e l
           )
        ) >>= fun () ->
      state.string_start_loc <- Location.none;
      Buffer.add_char state.buffer '"';
      comment state lexbuf }
  | "{" lowercase* "|"
      {
        let delim = Lexing.lexeme lexbuf in
        let delim = String.sub delim 1 (String.length delim - 2) in
        state.string_start_loc <- Location.curr lexbuf;
        Buffer.add_string state.buffer (Lexing.lexeme lexbuf);
        (catch (quoted_string state delim lexbuf) (fun e l -> match e with
             | Unterminated_string ->
               begin match state.comment_start_loc with
                 | [] -> assert false
                 | loc :: _ ->
                   let start = List.hd (List.rev state.comment_start_loc) in
                   state.comment_start_loc <- [];
                   fail (Unterminated_string_in_comment (start, l)) loc
               end
             | e -> fail e l
           )
        ) >>= fun () ->
        state.string_start_loc <- Location.none;
        Buffer.add_char state.buffer '|';
        Buffer.add_string state.buffer delim;
        Buffer.add_char state.buffer '}';
        comment state lexbuf }

  | "''"
      { Buffer.add_string state.buffer (Lexing.lexeme lexbuf); comment state lexbuf }
  | "'" newline "'"
      { update_loc lexbuf None 1 false 1;
        Buffer.add_string state.buffer (Lexing.lexeme lexbuf);
        comment state lexbuf
      }
  | "'" [^ '\\' '\'' '\010' '\013' ] "'"
      { Buffer.add_string state.buffer (Lexing.lexeme lexbuf); comment state lexbuf }
  | "'\\" ['\\' '"' '\'' 'n' 't' 'b' 'r' ' '] "'"
      { Buffer.add_string state.buffer (Lexing.lexeme lexbuf); comment state lexbuf }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { Buffer.add_string state.buffer (Lexing.lexeme lexbuf); comment state lexbuf }
  | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
      { Buffer.add_string state.buffer (Lexing.lexeme lexbuf); comment state lexbuf }
  | eof
      { match state.comment_start_loc with
        | [] -> assert false
        | loc :: _ ->
          let start = List.hd (List.rev state.comment_start_loc) in
          state.comment_start_loc <- [];
          fail (Unterminated_comment start) loc
      }
  | newline
      { update_loc lexbuf None 1 false 0;
        Buffer.add_string state.buffer (Lexing.lexeme lexbuf);
        comment state lexbuf
      }
  | _
      { Buffer.add_string state.buffer (Lexing.lexeme lexbuf); comment state lexbuf }

and string state = parse
    '"'
      { return () }
  | '\\' newline ([' ' '\t'] * as space)
      { update_loc lexbuf None 1 false (String.length space);
        string state lexbuf
      }
  | '\\' ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']
      { Buffer.add_char state.buffer
          (char_for_backslash (Lexing.lexeme_char lexbuf 1));
        string state lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { char_for_decimal_code state lexbuf 1 >>= fun c ->
        Buffer.add_char state.buffer c;
        string state lexbuf }
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
      { Buffer.add_char state.buffer (char_for_hexadecimal_code lexbuf 2);
        string state lexbuf }
  | '\\' _
      { if in_comment state
        then string state lexbuf
        else begin
(*  Should be an error, but we are very lax.
                  fail (Illegal_escape (Lexing.lexeme lexbuf),
                        (Location.curr lexbuf)
*)
          let loc = Location.curr lexbuf in
          Location.prerr_warning loc Warnings.Illegal_backslash;
          Buffer.add_char state.buffer (Lexing.lexeme_char lexbuf 0);
          Buffer.add_char state.buffer (Lexing.lexeme_char lexbuf 1);
          string state lexbuf
        end
      }
  | newline
      { if not (in_comment state) then
          Location.prerr_warning (Location.curr lexbuf) Warnings.Eol_in_string;
        update_loc lexbuf None 1 false 0;
        Buffer.add_string state.buffer (Lexing.lexeme lexbuf);
        string state lexbuf
      }
  | eof
      { let loc = state.string_start_loc in
        state.string_start_loc <- Location.none;
        fail Unterminated_string loc }
  | _
      { Buffer.add_char state.buffer (Lexing.lexeme_char lexbuf 0);
        string state lexbuf }

and quoted_string state delim = parse
  | newline
      { update_loc lexbuf None 1 false 0;
        Buffer.add_string state.buffer (Lexing.lexeme lexbuf);
        quoted_string state delim lexbuf
      }
  | eof
      { let loc = state.string_start_loc in
        state.string_start_loc <- Location.none;
        fail Unterminated_string loc }
  | "|" lowercase* "}"
      {
        let edelim = Lexing.lexeme lexbuf in
        let edelim = String.sub edelim 1 (String.length edelim - 2) in
        if delim = edelim then return ()
        else (Buffer.add_string state.buffer (Lexing.lexeme lexbuf);
              quoted_string state delim lexbuf)
      }
  | _
      { Buffer.add_char state.buffer (Lexing.lexeme_char lexbuf 0);
        quoted_string state delim lexbuf }

and skip_sharp_bang state = parse
  | "#!" [^ '\n']* '\n' [^ '\n']* "\n!#\n"
      { update_loc lexbuf None 3 false 0; token state lexbuf }
  | "#!" [^ '\n']* '\n'
      { update_loc lexbuf None 1 false 0; token state lexbuf }
  | "" { token state lexbuf }

and p4_quotation = parse
  | "<" (":" identchar*)? ("@" identchar*)? "<"
      { p4_quotation lexbuf }
  (* FIXME: This is fake *)
  | ">>"
      { return () }
  | newline
      { update_loc lexbuf None 1 false 0;
        p4_quotation lexbuf }
  | eof
      { fail Unterminated_string (Location.curr lexbuf) }
  | _
      { p4_quotation lexbuf }

{
  type comment = string * Location.t

  (* preprocessor support not implemented, not compatible with monadic
     interface *)

  let rec token_without_comments state lexbuf =
    token state lexbuf >>= function
    | COMMENT (s, comment_loc) ->
      token_without_comments state lexbuf
    | tok -> return tok
}
