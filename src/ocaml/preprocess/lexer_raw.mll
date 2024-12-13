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
open Misc
open Std
open Lexing
open Parser_raw

type keywords = (string, Parser_raw.token) Hashtbl.t

type error =
  | Illegal_character of char
  | Illegal_escape of string * string option
  | Reserved_sequence of string * string option
  | Unterminated_comment of Location.t
  | Unterminated_string
  | Unterminated_string_in_comment of Location.t * Location.t
  | Empty_character_literal
  | Keyword_as_label of string
  | Capitalized_label of string
  | Invalid_literal of string
  | Invalid_directive of string * string option
  | Invalid_encoding of string
  | Invalid_char_in_ident of Uchar.t
  | Non_lowercase_delimiter of string
  | Capitalized_raw_identifier of string
  | Unknown_keyword of string

exception Error of error * Location.t

(* Monad in which the lexer evaluates *)
type 'a result =
  | Return of 'a
  | Refill of (unit -> 'a result)
  | Fail of error * Location.t

let return a = Return a

let fail lexbuf e = Fail (e, Location.curr lexbuf)
let fail_loc e l = Fail (e,l)

let rec (>>=) (m : 'a result) (f : 'a -> 'b result) : 'b result =
  match m with
  | Return a -> f a
  | Refill u ->
    Refill (fun () -> u () >>= f)
  | Fail _ as e -> e

let (let*) = (>>=)
let (let+) = fun m f -> (>>=) m (fun x -> return (f x))

type preprocessor = (Lexing.lexbuf -> Parser_raw.token) -> Lexing.lexbuf -> Parser_raw.token

type state = {
  keywords: keywords;
  mutable buffer: Buffer.t;
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

let all_keywords =
  let v5_3 = Some (5,3) in
  let v1_0 = Some (1,0) in
  let v1_6 = Some (1,6) in
  let v4_2 = Some (4,2) in
  let always = None in
  [
    "and", AND, always;
    "as", AS, always;
    "assert", ASSERT, v1_6;
    "begin", BEGIN, always;
    "class", CLASS, v1_0;
    "constraint", CONSTRAINT, v1_0;
    "do", DO, always;
    "done", DONE, always;
    "downto", DOWNTO, always;
    "effect", EFFECT, v5_3;
    "else", ELSE, always;
    "end", END, always;
    "exception", EXCEPTION, always;
    "external", EXTERNAL, always;
    "false", FALSE, always;
    "for", FOR, always;
    "fun", FUN, always;
    "function", FUNCTION, always;
    "functor", FUNCTOR, always;
    "if", IF, always;
    "in", IN, always;
    "include", INCLUDE, always;
    "inherit", INHERIT, v1_0;
    "initializer", INITIALIZER, v1_0;
    "lazy", LAZY, v1_6;
    "let", LET, always;
    "match", MATCH, always;
    "method", METHOD, v1_0;
    "module", MODULE, always;
    "mutable", MUTABLE, always;
    "new", NEW, v1_0;
    "nonrec", NONREC, v4_2;
    "object", OBJECT, v1_0;
    "of", OF, always;
    "open", OPEN, always;
    "or", OR, always;
(*  "parser", PARSER; *)
    "private", PRIVATE, v1_0;
    "rec", REC, always;
    "sig", SIG, always;
    "struct", STRUCT, always;
    "then", THEN, always;
    "to", TO, always;
    "true", TRUE, always;
    "try", TRY, always;
    "type", TYPE, always;
    "val", VAL, always;
    "virtual", VIRTUAL, v1_0;
    "when", WHEN, always;
    "while", WHILE, always;
    "with", WITH, always;

    "lor", INFIXOP3("lor"), always; (* Should be INFIXOP2 *)
    "lxor", INFIXOP3("lxor"), always; (* Should be INFIXOP2 *)
    "mod", INFIXOP3("mod"), always;
    "land", INFIXOP3("land"), always;
    "lsl", INFIXOP4("lsl"), always;
    "lsr", INFIXOP4("lsr"), always;
    "asr", INFIXOP4("asr"), always
]

let keyword_table = Hashtbl.create 149

let populate_keywords (version,keywords) =
  let greater (x:(int*int) option) (y:(int*int) option) =
    match x, y with
    | None, _ | _, None -> true
    | Some x, Some y -> x >= y
  in
  let tbl = keyword_table in
  Hashtbl.clear tbl;
  let add_keyword (name, token, since) =
    if greater version since then Hashtbl.replace tbl name (Some token)
  in
  List.iter ~f:add_keyword all_keywords;
  List.iter ~f:(fun name ->
    match List.find ~f:(fun (n,_,_) -> n = name) all_keywords with
    | (_,tok,_) -> Hashtbl.replace tbl name (Some tok)
    | exception Not_found -> Hashtbl.replace tbl name None
    ) keywords

(* FIXME: Merlin: this could be made configurable *)
let () = populate_keywords (None,[])

let keywords l = create_hashtable 11 l

let list_keywords =
  let add_kw str _tok kws = str :: kws in
  let init = Hashtbl.fold add_kw keyword_table [] in
  fun keywords ->
    Hashtbl.fold add_kw keywords init

let store_string_char buf c = Buffer.add_char buf c
let store_string_utf_8_uchar buf u = Buffer.add_utf_8_uchar buf u
let store_string buf s = Buffer.add_string buf s
let store_substring buf s ~pos ~len = Buffer.add_substring buf s pos len

let store_lexeme buf lexbuf = store_string buf (Lexing.lexeme lexbuf)
let store_normalized_newline buf newline =
  (* #12502: we normalize "\r\n" to "\n" at lexing time,
     to avoid behavior difference due to OS-specific
     newline characters in string literals.

     (For example, Git for Windows will translate \n in versioned
     files into \r\n sequences when checking out files on Windows. If
     your code contains multiline quoted string literals, the raw
     content of the string literal would be different between Git for
     Windows users and all other users. Thanks to newline
     normalization, the value of the literal as a string constant will
     be the same no matter which programming tools are used.)

     Many programming languages use the same approach, for example
     Java, Javascript, Kotlin, Python, Swift and C++.
  *)
  (* Our 'newline' regexp accepts \r*\n, but we only wish
     to normalize \r?\n into \n -- see the discussion in #12502.
     All carriage returns except for the (optional) last one
     are reproduced in the output. We implement this by skipping
     the first carriage return, if any. *)
  let len = String.length newline in
  if len = 1
  then store_string_char buf '\n'
  else store_substring buf newline ~pos:1 ~len:(len - 1)

(* To store the position of the beginning of a string and comment *)
let in_comment state = state.comment_start_loc <> []
let print_warnings = ref true

(* Escaped chars are interpreted in strings unless they are in comments. *)
let store_escaped_char state lexbuf c =
  if in_comment state
  then store_lexeme state.buffer lexbuf
  else store_string_char state.buffer c

let store_escaped_uchar state lexbuf u =
  if in_comment state
  then store_lexeme state.buffer lexbuf
  else store_string_utf_8_uchar state.buffer u

let compute_quoted_string_idloc {Location.loc_start = orig_loc; _ } shift id =
  let id_start_pos = orig_loc.Lexing.pos_cnum + shift in
  let loc_start =
    Lexing.{orig_loc with pos_cnum = id_start_pos }
  in
  let loc_end =
    Lexing.{orig_loc with pos_cnum = id_start_pos + String.length id }
  in
  {Location. loc_start ; loc_end ; loc_ghost = false }

let wrap_string_lexer f state lexbuf =
  Buffer.reset state.buffer;
  state.string_start_loc <- Location.curr lexbuf;
  f state lexbuf >>= fun loc_end ->
  lexbuf.lex_start_p <- state.string_start_loc.Location.loc_start;
  let loc =
    Location.{
      loc_ghost = false;
      loc_start = state.string_start_loc.Location.loc_end;
      loc_end;
    }
  in
  state.string_start_loc <- Location.none;
  return (Buffer.contents state.buffer, loc)

let wrap_comment_lexer state comment lexbuf =
  let start_loc = Location.curr lexbuf  in
  state.comment_start_loc <- [start_loc];
  Buffer.reset state.buffer;
  let+ end_loc = comment state lexbuf in
  let s = Buffer.contents state.buffer in
  Buffer.reset state.buffer;
  s,
  { start_loc with Location.loc_end = end_loc.Location.loc_end }

(* to translate escape sequences *)

let digit_value c =
  match c with
  | 'a' .. 'f' -> 10 + Char.code c - Char.code 'a'
  | 'A' .. 'F' -> 10 + Char.code c - Char.code 'A'
  | '0' .. '9' -> Char.code c - Char.code '0'
  | _ -> assert false

let num_value lexbuf ~base ~first ~last =
  let c = ref 0 in
  for i = first to last do
    let v = digit_value (Lexing.lexeme_char lexbuf i) in
    assert(v < base);
    c := (base * !c) + v
  done;
  !c

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let illegal_escape lexbuf reason =
  let error = Illegal_escape (Lexing.lexeme lexbuf, Some reason) in
  fail lexbuf error

let char_for_decimal_code state lexbuf i =
  let c = num_value lexbuf ~base:10 ~first:i ~last:(i+2) in
  if (c < 0 || c > 255) then
    if in_comment state
    then return 'x'
    else
      illegal_escape lexbuf
        (Printf.sprintf
          "%d is outside the range of legal characters (0-255)." c)
  else return (Char.chr c)

let char_for_octal_code state lexbuf i =
  let c = num_value lexbuf ~base:8 ~first:i ~last:(i+2) in
  if (c < 0 || c > 255) then
    if in_comment state
    then return 'x'
    else
      illegal_escape lexbuf
        (Printf.sprintf
          "o%o (=%d) is outside the range of legal characters (0-255)." c c)
  else return (Char.chr c)

let char_for_hexadecimal_code lexbuf i =
  Char.chr (num_value lexbuf ~base:16 ~first:i ~last:(i+1))

let uchar_for_uchar_escape lexbuf =
  let illegal_escape lexbuf reason =
    let error = Illegal_escape (Lexing.lexeme lexbuf, Some reason) in
    raise (Error (error, Location.curr lexbuf))
  in
  let len = Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf in
  let first = 3 (* skip opening \u{ *) in
  let last = len - 2 (* skip closing } *) in
  let digit_count = last - first + 1 in
  match digit_count > 6 with
  | true ->
      illegal_escape lexbuf
        "too many digits, expected 1 to 6 hexadecimal digits"
  | false ->
      let cp = num_value lexbuf ~base:16 ~first ~last in
      if Uchar.is_valid cp then Uchar.unsafe_of_int cp else
      illegal_escape lexbuf
        (Printf.sprintf "%X is not a Unicode scalar value" cp)

let validate_encoding lexbuf raw_name =
  match Utf8_lexeme.normalize raw_name with
  | Error _ -> fail lexbuf (Invalid_encoding raw_name)
  | Ok name -> return name

let ident_for_extended lexbuf raw_name =
  let* name = validate_encoding lexbuf raw_name in
  match Utf8_lexeme.validate_identifier name with
  | Utf8_lexeme.Valid -> return name
  | Utf8_lexeme.Invalid_character u -> fail lexbuf (Invalid_char_in_ident u)
  | Utf8_lexeme.Invalid_beginning _ ->
  assert false (* excluded by the regexps *)

let validate_delim lexbuf raw_name =
  let* name = validate_encoding lexbuf raw_name in
  if Utf8_lexeme.is_lowercase name then return name
  else fail lexbuf (Non_lowercase_delimiter name)

let validate_ext lexbuf name =
  let* name = validate_encoding lexbuf name in
  match Utf8_lexeme.validate_identifier ~with_dot:true name with
  | Utf8_lexeme.Valid -> return name
  | Utf8_lexeme.Invalid_character u -> fail lexbuf (Invalid_char_in_ident u)
  | Utf8_lexeme.Invalid_beginning _ ->
  assert false (* excluded by the regexps *)

let lax_delim raw_name =
  match Utf8_lexeme.normalize raw_name with
  | Error _ -> None
  | Ok name ->
     if Utf8_lexeme.is_lowercase name then Some name
     else None

let keyword_or state s default =
  try Hashtbl.find state.keywords s
  with Not_found ->
    try Option.value ~default @@ Hashtbl.find keyword_table s
    with Not_found -> default

let is_keyword name =
  Hashtbl.mem keyword_table name

let () = Lexer.is_keyword_ref := is_keyword

let find_keyword lexbuf name default =
  match Hashtbl.find keyword_table name with
  | Some x -> return x
  | None -> fail lexbuf (Unknown_keyword name)
  | exception Not_found -> return default

let find_keyword state lexbuf ~name ~default =
  try return @@ Hashtbl.find state.keywords name
  with Not_found -> find_keyword lexbuf name default

let check_label_name ?(raw_escape=false) lexbuf name =
  if Utf8_lexeme.is_capitalized name then
    fail lexbuf (Capitalized_label name)
  else if not raw_escape && is_keyword name then
    fail lexbuf (Keyword_as_label name)
  else return name

(* Update the current location with file name and line number. *)

let update_loc lexbuf _file line absolute chars =
  let pos = lexbuf.lex_curr_p in
  let new_file = pos.pos_fname
    (*match file with
      | None -> pos.pos_fname
      | Some s -> s*)
  in
  lexbuf.lex_curr_p <- { pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }


(* TODO Merlin should we support this ?*)
let handle_docstrings = ref false

(* Error report *)

open Format_doc

let prepare_error loc = function
  | Illegal_character c ->
      Location.errorf ~loc "Illegal character (%s)" (Char.escaped c)
  | Illegal_escape (s, explanation) ->
      Location.errorf ~loc
        "Illegal backslash escape in string or character (%s)%t" s
        (fun ppf -> match explanation with
           | None -> ()
           | Some expl -> fprintf ppf ": %s" expl)
  | Reserved_sequence (s, explanation) ->
      Location.errorf ~loc
        "Reserved character sequence: %s%t" s
        (fun ppf -> match explanation with
           | None -> ()
           | Some expl -> fprintf ppf " %s" expl)
  | Unterminated_comment _ ->
      Location.errorf ~loc "Comment not terminated"
  | Unterminated_string ->
      Location.errorf ~loc "String literal not terminated"
  | Unterminated_string_in_comment (_, literal_loc) ->
      Location.errorf ~loc
        "This comment contains an unterminated string literal"
        ~sub:[Location.msg ~loc:literal_loc "String literal begins here"]
  | Empty_character_literal ->
      let msg = "Illegal empty character literal ''" in
      let sub =
        [Location.msg
           "Hint: Did you mean ' ' or a type variable 'a?"] in
      Location.error ~loc ~sub msg
  | Keyword_as_label kwd ->
      Location.errorf ~loc
        "%a is a keyword, it cannot be used as label name" Style.inline_code kwd
  | Capitalized_label lbl ->
      Location.errorf ~loc
        "%a cannot be used as label name, \
         it must start with a lowercase letter" Style.inline_code lbl
  | Invalid_literal s ->
      Location.errorf ~loc "Invalid literal %s" s
  | Invalid_directive (dir, explanation) ->
      Location.errorf ~loc "Invalid lexer directive %S%t" dir
        (fun ppf -> match explanation with
           | None -> ()
           | Some expl -> fprintf ppf ": %s" expl)
  | Invalid_encoding s ->
    Location.errorf ~loc "Invalid encoding of identifier %s." s
  | Invalid_char_in_ident u ->
      Location.errorf ~loc "Invalid character U+%X in identifier"
         (Uchar.to_int u)
  | Capitalized_raw_identifier lbl ->
      Location.errorf ~loc
        "%a cannot be used as a raw identifier, \
         it must start with a lowercase letter" Style.inline_code lbl
  | Non_lowercase_delimiter name ->
      Location.errorf ~loc
        "%a cannot be used as a quoted string delimiter,@ \
         it must contain only lowercase letters."
         Style.inline_code name
  | Unknown_keyword name ->
      Location.errorf ~loc
      "%a has been defined as an additional keyword.@ \
       This version of OCaml does not support this keyword."
      Style.inline_code name
(* FIXME: Invalid_directive? *)

let () =
  Location.register_error_of_exn
    (function
      | Error (err, loc) ->
        Some (prepare_error loc err)
      | _ ->
        None
    )

}

let newline = ('\013'* '\010')
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '_']
let uppercase = ['A'-'Z']
let identstart = lowercase | uppercase
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']
let utf8 = ['\192'-'\255'] ['\128'-'\191']*
let identstart_ext = identstart | utf8
let identchar_ext = identchar | utf8
let delim_ext = (lowercase | uppercase | utf8)*
(* ascii uppercase letters in quoted string delimiters ({delim||delim}) are
   rejected by the delimiter validation function, we accept them temporarily to
   have the same error message for ascii and non-ascii uppercase letters *)

(* TODO REMOVE *)
let lowercase_latin1 = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase_latin1 = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar_latin1 = identchar
  (*['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']*)
(* END TODO REMOVE *)

let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let symbolcharnopercent = (* TODO ???? *)
  ['!' '$' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let dotsymbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '/' ':' '=' '>' '?' '@' '^' '|']
let symbolchar_or_hash =
  symbolchar | '#'
let kwdopchar =
  ['$' '&' '*' '+' '-' '/' '<' '=' '>' '@' '^' '|']

let ident = (lowercase | uppercase) identchar*
let ident_ext = identstart_ext  identchar_ext*
let extattrident = ident ('.' ident)*

let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let hex_digit =
  ['0'-'9' 'A'-'F' 'a'-'f']
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
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*) ?
let hex_float_literal =
  '0' ['x' 'X']
  ['0'-'9' 'A'-'F' 'a'-'f'] ['0'-'9' 'A'-'F' 'a'-'f' '_']*
  ('.' ['0'-'9' 'A'-'F' 'a'-'f' '_']* )?
  (['p' 'P'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?
let literal_modifier = ['G'-'Z' 'g'-'z']
let raw_ident_escape = "\\#"


refill {fun k lexbuf -> Refill (fun () -> k lexbuf)}


rule token state = parse
  | ("\\" as bs) newline {
      match state.preprocessor with
      | None -> fail lexbuf (Illegal_character bs)
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
      { return METAOCAML_BRACKET_OPEN }
  | ">."
      { return (keyword_or state (Lexing.lexeme lexbuf) (INFIXOP0 ">.")) }
  | ".~"
      { return (keyword_or state (Lexing.lexeme lexbuf) METAOCAML_ESCAPE) }
  | "_"
      { return UNDERSCORE }
  | "~"
      { return TILDE }
      (*
  | ".~"
      { fail lexbuf
          (Reserved_sequence (".~", Some "is reserved for use in MetaOCaml")) }
      *)
  | "~" (identstart identchar * as name) ':'
      { lABEL (check_label_name lexbuf name) }
  | "~" (raw_ident_escape? as escape) (ident_ext as raw_name) ':'
      { ident_for_extended lexbuf raw_name
        >>= check_label_name ~raw_escape:(escape<>"") lexbuf
        |> lABEL }
  | "?"
      { return QUESTION }
  | "?" (lowercase identchar * as name) ':'
      { oPTLABEL (check_label_name lexbuf name) }
  | "?" (raw_ident_escape? as escape) (ident_ext as raw_name) ':'
      { ident_for_extended lexbuf raw_name
        >>= check_label_name ~raw_escape:(escape<>"") lexbuf
        |> oPTLABEL }
  (* | raw_ident_escape (lowercase identchar * as name)
      { return (LIDENT name) } *)
  | lowercase identchar * as name
    { (find_keyword state lexbuf ~name ~default:(LIDENT name)) }
  | uppercase identchar * as name
    { (* Capitalized keywords for OUnit *)
      (find_keyword state lexbuf ~name ~default:(UIDENT name))}
  | (raw_ident_escape? as escape) (ident_ext as raw_name)
    { let* name = ident_for_extended lexbuf raw_name in
      if Utf8_lexeme.is_capitalized name then begin
        if escape="" then return (UIDENT name)
        else return (UIDENT name)
          (* we don't have capitalized keywords, and thus no needs for
             capitalized raw identifiers. *)
          (*fail lexbuf (Capitalized_raw_identifier name)*)
      end else return (LIDENT name) }
  | int_literal as lit { return (INT (lit, None)) }
  | (int_literal as lit) (literal_modifier as modif)
    { return (INT (lit, Some modif)) }
  | float_literal | hex_float_literal as lit
    { return (FLOAT (lit, None)) }
  | (float_literal | hex_float_literal as lit) (literal_modifier as modif)
    { return (FLOAT (lit, Some modif)) }
  | (float_literal | hex_float_literal | int_literal) identchar+ as invalid
    { fail lexbuf (Invalid_literal invalid) }
  | "\""
      { wrap_string_lexer string state lexbuf >>= fun (str, loc) ->
        return (STRING (str, loc, None)) }
  | "{" (delim_ext as raw_name) '|'
      { let* delim = validate_delim lexbuf raw_name in
        let+ s, loc = wrap_string_lexer (quoted_string delim) state lexbuf in
        STRING (s, loc, Some delim) }
  | "{%" (extattrident as raw_id) "|"
      { let orig_loc = Location.curr lexbuf in
        let* id = validate_ext lexbuf raw_id in
        let+ s, loc =wrap_string_lexer (quoted_string "") state lexbuf in
        let idloc = compute_quoted_string_idloc orig_loc 2 id in
        QUOTED_STRING_EXPR (id, idloc, s, loc, Some "") }
  | "{%" (extattrident as raw_id) blank+ (delim_ext as raw_delim) "|"
      { let orig_loc = Location.curr lexbuf in
        let* id = validate_ext lexbuf raw_id in
        let* delim = validate_delim lexbuf raw_delim in
        let+ s, loc = wrap_string_lexer (quoted_string delim) state lexbuf in
        let idloc = compute_quoted_string_idloc orig_loc 2 id in
        QUOTED_STRING_EXPR (id, idloc, s, loc, Some delim) }
  | "{%%" (extattrident as raw_id) "|"
      { let orig_loc = Location.curr lexbuf in
        let* id = validate_ext lexbuf raw_id in
        let+ s, loc = wrap_string_lexer (quoted_string "") state lexbuf in
        let idloc = compute_quoted_string_idloc orig_loc 3 id in
        QUOTED_STRING_ITEM (id, idloc, s, loc, Some "") }
  | "{%%" (extattrident as raw_id) blank+ (delim_ext as raw_delim) "|"
      { let orig_loc = Location.curr lexbuf in
        let* id = validate_ext lexbuf raw_id in
        let* delim = validate_delim lexbuf raw_delim in
        let+ s, loc = wrap_string_lexer (quoted_string delim) state lexbuf in
        let idloc = compute_quoted_string_idloc orig_loc 3 id in
        QUOTED_STRING_ITEM (id, idloc, s, loc, Some delim) }
  | "\'" newline "\'"
    { update_loc lexbuf None 1 false 1;
      (* newline is ('\013'* '\010') *)
      return (CHAR '\n') }
  | "\'" ([^ '\\' '\'' '\010' '\013'] as c) "\'"
    { return (CHAR c) }
  | "\'\\" (['\\' '\'' '\"' 'n' 't' 'b' 'r' ' '] as c) "\'"
    { return (CHAR (char_for_backslash c)) }
  | "\'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "\'"
    { char_for_decimal_code state lexbuf 2 >>= fun c -> return (CHAR c) }
  | "\'\\" 'o' ['0'-'3'] ['0'-'7'] ['0'-'7'] "\'"
    { char_for_octal_code state lexbuf 3 >>= fun c -> return (CHAR c) }
  | "\'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "\'"
    { return (CHAR (char_for_hexadecimal_code lexbuf 3)) }
  | "\'" ("\\" [^ '#'] as esc)
      { fail lexbuf (Illegal_escape (esc, None)) }
  | "(*"
      { let+ s, loc = wrap_comment_lexer state comment lexbuf in
        COMMENT (s, loc) }
  | "(**"
      { let+ s, loc = wrap_comment_lexer state comment lexbuf in
        if !handle_docstrings then
          DOCSTRING (Docstrings.docstring s loc)
        else
          COMMENT ("*" ^ s, loc)
      }
  | "(**" (('*'+) as stars)
      { let+ s, loc =
          wrap_comment_lexer
            state
            (fun state lexbuf ->
               store_string state.buffer ("*" ^ stars);
               comment state  lexbuf)
            lexbuf
        in
        COMMENT (s, loc) }
  | "(*)"
      { if !print_warnings then
          Location.prerr_warning (Location.curr lexbuf) Warnings.Comment_start;
        let+ s, loc = wrap_comment_lexer state comment lexbuf in
        COMMENT (s, loc) }
  | "(*" (('*'*) as stars) "*)"
      { if !handle_docstrings && stars="" then
         (* (**) is an empty docstring *)
          return (DOCSTRING(Docstrings.docstring "" (Location.curr lexbuf)))
        else
          return (COMMENT (stars, Location.curr lexbuf)) }
  | "*)"
      { let loc = Location.curr lexbuf in
        Location.prerr_warning loc Warnings.Comment_not_end;
        lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - 1;
        let curpos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- { curpos with pos_cnum = curpos.pos_cnum - 1 };
        return STAR
      }
  | "#"
      { let at_beginning_of_line pos = (pos.pos_cnum = pos.pos_bol) in
        if not (at_beginning_of_line lexbuf.lex_start_p)
        then return HASH
        else try directive state lexbuf with Failure _ -> return HASH
      }
  | "&"  { return AMPERSAND }
  | "&&" { return AMPERAMPER }
  | "`"  { return BACKQUOTE }
  | "\'" { return QUOTE }
  | "("  { return LPAREN }
  | ")"  { return RPAREN }
  | "*"  { return STAR }
  | ","  { return COMMA }
  | "->" { return MINUSGREATER }
  | "."  { return DOT }
  | "." (dotsymbolchar symbolchar* as op) { return (DOTOP op) }
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
  | "[@@"  { return LBRACKETATAT }
  | "[@@@" { return LBRACKETATATAT }
  | "[%" { return LBRACKETPERCENT }
  | "[%%" { return LBRACKETPERCENTPERCENT }
  | "!"  { return BANG }
  | "!=" { return (INFIXOP0 "!=") }
  | "+"  { return PLUS }
  | "+." { return PLUSDOT }
  | "+=" { return PLUSEQ }
  | "-"  { return MINUS }
  | "-." { return MINUSDOT }

  | "!" symbolchar_or_hash + as op
            { return (PREFIXOP op) }
  | ['~' '?'] symbolchar_or_hash + as op
            { return (PREFIXOP op) }
  | ['=' '<' '>' '|' '&' '$'] symbolchar * as op
            { return (keyword_or state op
                       (INFIXOP0 op)) }
  | ['@' '^'] symbolchar * as op
            { return (INFIXOP1 op) }
  | ['+' '-'] symbolchar * as op
            { return (INFIXOP2 op) }
  | "**" symbolchar * as op
            { return (INFIXOP4 op) }
  | '%'     { return PERCENT }
  | ['*' '/' '%'] symbolchar * as op
            { return (INFIXOP3 op) }
  (* Old style js_of_ocaml support is implemented by generating a custom token *)
  | '#' symbolchar_or_hash + as op
            { return (try Hashtbl.find state.keywords op
                      with Not_found -> HASHOP op) }
  | "let" kwdopchar dotsymbolchar * as op
            { return (LETOP op) }
  | "and" kwdopchar dotsymbolchar * as op
            { return (ANDOP op) }
  | eof { return EOF }

  | _ as illegal_char
      { fail lexbuf (Illegal_character illegal_char) }

and directive state = parse
  | ([' ' '\t']* (['0'-'9']+ as num) [' ' '\t']*
        ("\"" ([^ '\010' '\013' '\"' ] * as name) "\"") as directive)
        [^ '\010' '\013'] *
      {
        match int_of_string num with
        | exception _ ->
            (* PR#7165 *)
            let explanation = "line number out of range" in
            fail lexbuf (Invalid_directive ("#" ^ directive, Some explanation))
        | line_num ->
           (* Documentation says that the line number should be
              positive, but we have never guarded against this and it
              might have useful hackish uses. *)
            update_loc lexbuf (Some name) (line_num - 1) true 0;
            token state lexbuf
      }
and comment state = parse
    "(*"
      { state.comment_start_loc <- (Location.curr lexbuf) :: state.comment_start_loc;
        store_lexeme state.buffer lexbuf;
        comment state lexbuf
      }
  | "*)"
      { match state.comment_start_loc with
        | [] -> assert false
        | [_] -> state.comment_start_loc <- []; return (Location.curr lexbuf)
        | _ :: l -> state.comment_start_loc <- l;
                  store_lexeme state.buffer lexbuf;
                  comment state lexbuf
       }
  | "\""
      {
        state.string_start_loc <- Location.curr lexbuf;
        Buffer.add_char state.buffer '\"';
        let buffer = state.buffer in
        state.buffer <- Buffer.create 15;
        (catch (string state lexbuf) (fun e l -> match e with
             | Unterminated_string ->
               begin match state.comment_start_loc with
                 | [] -> assert false
                 | loc :: _ ->
                   let start = List.hd (List.rev state.comment_start_loc) in
                   state.comment_start_loc <- [];
                   fail_loc (Unterminated_string_in_comment (start, l)) loc
               end
             | e -> fail_loc e l
           )
        ) >>= fun _loc ->
        state.string_start_loc <- Location.none;
        Buffer.add_string buffer (String.escaped (Buffer.contents state.buffer));
        state.buffer <- buffer;
        store_string_char state.buffer '\"';
        comment state lexbuf }
  | "{" ('%' '%'? extattrident blank*)? (delim_ext as raw_delim) "|"
      { match lax_delim raw_delim with
        | None -> store_lexeme state.buffer lexbuf; comment state lexbuf
        | Some delim ->
          state.string_start_loc <- Location.curr lexbuf;
          Buffer.add_string state.buffer (Lexing.lexeme lexbuf);
          (catch (quoted_string delim state lexbuf) (fun e l -> match e with
              | Unterminated_string ->
                begin match state.comment_start_loc with
                  | [] -> assert false
                  | loc :: _ ->
                    let start = List.hd (List.rev state.comment_start_loc) in
                    state.comment_start_loc <- [];
                    fail_loc (Unterminated_string_in_comment (start, l)) loc
                end
              | e -> fail_loc e l
            )
          ) >>= fun _loc ->
          state.string_start_loc <- Location.none;
          Buffer.add_char state.buffer '|';
          Buffer.add_string state.buffer delim;
          Buffer.add_char state.buffer '}';
          comment state lexbuf }

  | "\'\'"
      { store_lexeme state.buffer lexbuf; comment state lexbuf }
  | "\'" (newline as nl) "\'"
      { update_loc lexbuf None 1 false 1;
        store_string_char state.buffer '\'';
        store_normalized_newline state.buffer nl;
        store_string_char state.buffer '\'';
        comment state lexbuf
      }
  | "\'" [^ '\\' '\'' '\010' '\013' ] "\'"
      { store_lexeme state.buffer lexbuf; comment state lexbuf }
  | "\'\\" ['\\' '\"' '\'' 'n' 't' 'b' 'r' ' '] "\'"
      { store_lexeme state.buffer lexbuf; comment state lexbuf }
  | "\'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "\'"
      { store_lexeme state.buffer lexbuf; comment state lexbuf }
  | "\'\\" 'o' ['0'-'3'] ['0'-'7'] ['0'-'7'] "\'"
      { store_lexeme state.buffer lexbuf; comment state lexbuf }
  | "\'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "\'"
      { store_lexeme state.buffer lexbuf; comment state lexbuf }
  | eof
      { match state.comment_start_loc with
        | [] -> assert false
        | loc :: _ ->
          let start = List.hd (List.rev state.comment_start_loc) in
          state.comment_start_loc <- [];
          fail_loc (Unterminated_comment start) loc
      }
  | newline as nl
      { update_loc lexbuf None 1 false 0;
        store_normalized_newline state.buffer nl;
        comment state lexbuf
      }
  | ident
      { store_lexeme state.buffer lexbuf; comment state lexbuf }
  | _
      { store_lexeme state.buffer lexbuf; comment state lexbuf }

and string state = parse
    '\"'
      { return lexbuf.lex_start_p  }
  | '\\' (newline as nl) ([' ' '\t'] * as space)
      { update_loc lexbuf None 1 false (String.length space);
        if in_comment state then begin
          store_string_char state.buffer '\\';
          store_normalized_newline state.buffer nl;
          store_string state.buffer space;
        end;
        string state lexbuf
      }
  | '\\' (['\\' '\'' '\"' 'n' 't' 'b' 'r' ' '] as c)
      { store_escaped_char state lexbuf (char_for_backslash c);
        string state lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { char_for_decimal_code state lexbuf 1 >>= fun c ->
        store_escaped_char state lexbuf c;
         string state lexbuf }
  | '\\' 'o' ['0'-'7'] ['0'-'7'] ['0'-'7']
      { char_for_octal_code state lexbuf 2 >>= fun c ->
        store_escaped_char state lexbuf c;
         string state lexbuf }
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
      { store_escaped_char state lexbuf (char_for_hexadecimal_code lexbuf 2);
         string state lexbuf }
  | '\\' 'u' '{' hex_digit+ '}'
      { store_escaped_uchar state lexbuf (uchar_for_uchar_escape lexbuf);
        string state lexbuf }
  | '\\' _
      { if in_comment state
        then string state lexbuf
        else begin
(*  Should be an error, but we are very lax.
          error lexbuf (Illegal_escape (Lexing.lexeme lexbuf, None))
*)
          let loc = Location.curr lexbuf in
          Location.prerr_warning loc Warnings.Illegal_backslash;
          store_lexeme state.buffer lexbuf;
          string state lexbuf
        end
      }
  | newline as nl
      { update_loc lexbuf None 1 false 0;
        store_normalized_newline state.buffer nl;
        string state lexbuf
      }
  | eof
      { let loc = state.string_start_loc in
        state.string_start_loc <- Location.none;
        fail_loc Unterminated_string loc }
  | _
      { Buffer.add_char state.buffer (Lexing.lexeme_char lexbuf 0);
        string state lexbuf }

and quoted_string delim state = parse
  | newline as nl
      { update_loc lexbuf None 1 false 0;
        store_normalized_newline state.buffer nl;
        quoted_string delim state lexbuf
      }
  | eof
      { let loc = state.string_start_loc in
        state.string_start_loc <- Location.none;
        fail_loc Unterminated_string loc }
  | "|" (ident_ext? as raw_edelim) "}"
      {
        let* edelim = validate_encoding lexbuf raw_edelim in
        if delim = edelim then return lexbuf.lex_start_p
        else (store_lexeme state.buffer lexbuf;
              quoted_string delim state lexbuf)
      }
  | (_ as c)
      { store_string_char state.buffer c;
        quoted_string delim state lexbuf }

and skip_sharp_bang state = parse
  | "#!" [^ '\n']* '\n' [^ '\n']* "\n!#\n"
      { update_loc lexbuf None 3 false 0; token state lexbuf }
  | "#!" [^ '\n']* '\n'
      { update_loc lexbuf None 1 false 0; token state lexbuf }
  | "" { token state lexbuf }

{
  type comment = string * Location.t

  (* preprocessor support not implemented, not compatible with monadic
     interface *)

  let rec token_without_comments state lexbuf =
    token state lexbuf >>= function
    | COMMENT _ ->
      token_without_comments state lexbuf
    | tok -> return tok
}
