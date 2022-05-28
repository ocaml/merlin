(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

{

open Lexing
open Menhir_def
open Menhir_parser
open Positions
open Keyword

(* ------------------------------------------------------------------------ *)

(* Short-hands. *)

let error1 pos =
  Error.error (Positions.one pos)

let error2 lexbuf =
  Error.error (Positions.lexbuf lexbuf)

(* ------------------------------------------------------------------------ *)

(* [int_of_string] raises [Failure] if its argument is too large. This is
   not a problem in practice, but causes false positives when fuzzing
   Menhir. We hide the problem by failing gracefully. *)

let int_of_string (pos : Lexing.position) i =
  try
    int_of_string i
  with Failure _ ->
    error1 pos "unreasonably large integer."

(* ------------------------------------------------------------------------ *)

(* This wrapper saves the current lexeme start, invokes its argument,
   and restores it. This allows transmitting better positions to the
   parser. *)

let savestart lexbuf f =
  let startp = lexbuf.lex_start_p in
  let token = f lexbuf in
  lexbuf.lex_start_p <- startp;
  token

(* ------------------------------------------------------------------------ *)

(* Overwrites an old character with a new one at a specified
   offset in a [bytes] buffer. *)

let overwrite content offset c1 c2 =
  assert (Bytes.get content offset = c1);
  Bytes.set content offset c2

(* ------------------------------------------------------------------------ *)

(* Keyword recognition and construction. *)

(* A monster is a spot where we have identified a keyword in concrete syntax.
   We describe a monster as an object with the following methods: *)

type monster = {

  (* The position of the monster. *)
  pos: Positions.t;

  (* This method is passed an array of (optional) names for the producers,
     that is, the elements of the production's right-hand side. It is also
     passed a flag which tells whether [$i] syntax is allowed or disallowed.
     It may perform some checks and is allowed to fail. *)
  check: check;

  (* This method transforms the keyword (in place) into a conventional
     OCaml identifier. This is done by replacing '$', '(', and ')' with
     '_'. Bloody. The arguments are [ofs1] and [content]. [ofs1] is the
     offset where [content] begins in the source file. *)
  transform: int -> bytes -> unit;

  (* This is the keyword, in abstract syntax. *)
  keyword: keyword option;

  (* If this is a [$i] monster, then the identifier [_i] is stored here. *)
  oid: string option;

}

(* The reason why we parameterize [check] over [Settings.dollars], instead
   of reading this setting directly, is that [NewRuleSyntax] calls [check]
   and wants to disallow dollars. *)
and check =
  (* Settings.dollars -> DollarsAllowed *)
  string option array -> unit

(* No check. *)

let none : check =
  fun _ -> ()

(* ------------------------------------------------------------------------ *)

(* We check that every [$i] is within range. Also, we forbid using [$i]
   when a producer has been given a name; this is bad style and may be
   a mistake. (Plus, this simplifies our life, as we rewrite [$i] to [_i],
   and we would have to rewrite it to a different identifier otherwise.) *)

let check_dollar pos i : check = fun producers ->
  (* If [i] is out of range, say so. *)
  if not (0 <= i - 1 && i - 1 < Array.length producers) then
    Error.error [pos] "$%d refers to a nonexistent symbol." i;
  (* If [$i] could be referred to via a name, say so. *)
  producers.(i - 1) |> Option.iter (fun x ->
    Error.error [pos] "please do not say: $%d. Instead, say: %s." i x
  )
  (* If [$i] syntax is disallowed, say so. *)
  (*match dollars with
  | Settings.DollarsDisallowed ->
      Error.error [pos] "please do not use $%d. Instead, name this value." i
  | Settings.DollarsAllowed ->
      ()*)

(* We check that every reference to a producer [x] in a position keyword,
   such as [$startpos(x)], exists. *)

let check_producer pos x : check = fun producers ->
  if not (List.mem (Some x) (Array.to_list producers)) then
    Error.error [pos] "%s refers to a nonexistent symbol." x

(* ------------------------------------------------------------------------ *)

(* The [$i] monster. *)

let dollar pos i : monster =
  let check : check = check_dollar pos i
  and transform ofs1 content =
    (* [$i] is replaced with [_i]. Thus, it is no longer a keyword. *)
    let pos = start_of_position pos in
    let ofs = pos.pos_cnum - ofs1 in
    overwrite content ofs '$' '_'
  and keyword =
    None
  and oid =
    Some (Printf.sprintf "_%d" i)
    in
  { pos; check; transform; keyword; oid }

(* ------------------------------------------------------------------------ *)

(* The position-keyword monster. The most horrible of all. *)

let position pos
  (where : string)
  (flavor : string)
  (i : string option) (x : string option)
=
  let check_no_parameter () =
    if i <> None || x <> None then
      Error.error [pos] "$%s%s does not take a parameter." where flavor
  in
  let ofslpar = (* offset of the opening parenthesis, if there is one *)
    1 + (* for the initial "$" *)
    String.length where +
    3   (* for "pos" or "ofs" or "loc" *)
  in
  let where =
    match where with
    | "symbolstart"
    | "s"           -> check_no_parameter(); WhereSymbolStart
    | "start"       -> WhereStart
    | "end"         -> WhereEnd
    | ""            -> WhereStart
    | _             -> assert false
  in
  let flavor =
    match flavor with
    | "pos"   -> FlavorPosition
    | "ofs"   -> FlavorOffset
    | "loc"   -> FlavorLocation
    | _       -> assert false
  in
  let subject, check =
    match i, x with
    | Some i, None ->
        let ii = int_of_string (start_of_position pos) i in
        if ii = 0 && where = WhereEnd then
          (* [$endpos($0)] *)
          Before, none
        else
          (* [$startpos($i)] is rewritten to [$startpos(_i)]. *)
          RightNamed ("_" ^ i), check_dollar pos ii
    | None, Some x ->
        (* [$startpos(x)] *)
        RightNamed x, check_producer pos x
    | None, None ->
        (* [$startpos] *)
        Left, none
    | Some _, Some _ ->
        assert false
  in
  let transform ofs1 content =
    let pos = start_of_position pos in
    let ofs = pos.pos_cnum - ofs1 in
    overwrite content ofs '$' '_';
    let ofslpar = ofs + ofslpar in
    match i, x with
    | None, Some x ->
        overwrite content ofslpar '(' '_';
        overwrite content (ofslpar + 1 + String.length x) ')' '_'
    | Some i, None ->
        overwrite content ofslpar '(' '_';
        overwrite content (ofslpar + 1) '$' '_';
        overwrite content (ofslpar + 2 + String.length i) ')' '_'
    | _, _ ->
        ()
  in
  let keyword =
    Some (Position (subject, where, flavor))
  and oid =
    None
  in
  { pos; check; transform; keyword; oid }

(* ------------------------------------------------------------------------ *)

(* In an OCaml header, there should be no monsters. This is just a sanity
   check. *)

let no_monsters monsters =
  match monsters with
  | [] ->
      ()
  | monster :: _ ->
      Error.error [monster.pos]
        "a Menhir keyword cannot be used in an OCaml header."

(* ------------------------------------------------------------------------ *)

(* Gathering all of the identifiers in an array of optional identifiers. *)

let gather_oid xs oid =
  match oid with
  | Some x ->
      StringSet.add x xs
  | None ->
      xs

let gather_oids oids =
  Array.fold_left gather_oid StringSet.empty oids

(* Gathering all of the [oid] identifiers in a list of monsters. *)

let gather_monsters monsters =
  List.fold_left (fun xs monster ->
    gather_oid xs monster.oid
  ) StringSet.empty monsters

(* ------------------------------------------------------------------------ *)

(* Creates a stretch. *)

let mk_stretch pos1 pos2 parenthesize monsters =
  (* Read the specified chunk of the file. *)
  let raw_content : string = InputFile.chunk (pos1, pos2) in
  (* Transform the monsters, if there are any. (This explicit test
     allows saving one string copy and keeping just one live copy.) *)
  let content : string =
    match monsters with
    | [] ->
        raw_content
    | _ :: _ ->
        let content : bytes = Bytes.of_string raw_content in
        List.iter (fun monster -> monster.transform pos1.pos_cnum content) monsters;
        Bytes.unsafe_to_string content
  in
  (* Add whitespace so that the column numbers match those of the source file.
     If requested, add parentheses so that the semantic action can be inserted
     into other code without ambiguity. *)
  let content =
    if parenthesize then
      (* If [parenthesize] is true then we are at the beginning of a semantic
         action, just after the opening brace. This guarantees that we cannot
         be at the beginning of a line, so the subtraction [_ - 1] below
         cannot produce a negative result. *)
      (String.make (pos1.pos_cnum - pos1.pos_bol - 1) ' ') ^ "(" ^ content ^ ")"
    else
      (String.make (pos1.pos_cnum - pos1.pos_bol) ' ') ^ content
  in
  { Stretch.
    filename = InputFile.get_input_file_name();
    linenum = pos1.pos_lnum;
    linecount = pos2.pos_lnum - pos1.pos_lnum;
    content = content;
    raw_content = raw_content;
    keywords = List.filter_map (fun monster -> monster.keyword) monsters
  }

(* Creating a stretch from a located identifier. (This does not require the
   input file to be currently opened.) In this variant, [parenthesize] is
   false, [monsters] is empty. *)

let stretch_of_id (id : string located) =
  let raw_content, pos = Positions.decompose id in
  let pos1 = Positions.start_of_position pos
  and pos2 = Positions.end_of_position pos
  and filename = Positions.filename_of_position pos in
  assert (pos1 != Lexing.dummy_pos);
  let padding = pos1.pos_cnum - pos1.pos_bol in
  let content = String.make padding ' ' ^ raw_content in
  { Stretch.
    filename = filename;
    linenum = pos1.pos_lnum;
    linecount = pos2.pos_lnum - pos1.pos_lnum;
    content = content;
    raw_content = raw_content;
    keywords = []
  }

(* ------------------------------------------------------------------------ *)

(* OCaml's reserved words. *)

let table words =
  let table = Hashtbl.create 149 in
  List.iter (fun word -> Hashtbl.add table word ()) words;
  table

let reserved =
  table [
    "and";
    "as";
    "assert";
    "begin";
    "class";
    "constraint";
    "do";
    "done";
    "downto";
    "else";
    "end";
    "exception";
    "external";
    "false";
    "for";
    "fun";
    "function";
    "functor";
    "if";
    "in";
    "include";
    "inherit";
    "initializer";
    "lazy";
    "let";
    "match";
    "method";
    "module";
    "mutable";
    "new";
    "object";
    "of";
    "open";
    "or";
    "parser";
    "private";
    "rec";
    "sig";
    "struct";
    "then";
    "to";
    "true";
    "try";
    "type";
    "val";
    "virtual";
    "when";
    "while";
    "with";
    "mod";
    "land";
    "lor";
    "lxor";
    "lsl";
    "lsr";
    "asr";
  ]

(* ------------------------------------------------------------------------ *)

(* Menhir's percent-directives. *)

let table directives =
  let table = Hashtbl.create 149 in
  List.iter (fun (word, token) -> Hashtbl.add table word token) directives;
  table

let directives =
  table [
    "token", TOKEN;
    "type", TYPE;
    "left", LEFT;
    "right", RIGHT;
    "nonassoc", NONASSOC;
    "start", START;
    "prec", PREC;
    "public", PUBLIC;
    "parameter", PARAMETER;
    "inline", INLINE;
    "attribute", PERCENTATTRIBUTE;
    "on_error_reduce", ON_ERROR_REDUCE;
  ]

(* ------------------------------------------------------------------------ *)

(* Decoding escaped characters. *)

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

}

(* ------------------------------------------------------------------------ *)

(* Patterns. *)

let newline = ('\010' | '\013' | "\013\010")

let whitespace = [ ' ' '\t' ]

let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']

let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']

let identchar = ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '0'-'9'] (* '\'' forbidden *)

let attributechar = identchar | '.'

let subject =
  '$' (['0'-'9']+ as i)
| ((lowercase identchar*) as x)

let poskeyword =
  '$'
  (
    (("symbolstart" | "start" | "end") as where) (("pos" | "ofs") as flavor)
  | (("s" | "") as where) ("loc" as flavor)
  )
  ( '(' subject ')' )?

let previouserror =
  "$previouserror"

let syntaxerror =
  "$syntaxerror"

(* ------------------------------------------------------------------------ *)

(* The lexer. *)

rule main = parse
| "%" (identchar+ as directive)
    { try Hashtbl.find directives directive
      with Not_found -> error2 lexbuf "unknown directive: %s." directive }
| "%%"
    { (* The token [PERCENTPERCENT] carries a stretch that contains
         everything that follows %% in the input file. This string
         must be created lazily. The parser decides (based on the
         context) whether this stretch is needed. If it is indeed
         needed, then constructing this stretch drives the lexer
         to the end of the file. *)
      PERCENTPERCENT (lazy (
        let openingpos = lexeme_end_p lexbuf in
        let closingpos = finish lexbuf in
        mk_stretch openingpos closingpos false []
      )) }
| ";"
    { SEMI }
| ":"
    { COLON }
| ","
    { COMMA }
| "="
    { EQUAL }
| "("
    { LPAREN }
| ")"
    { RPAREN }
| "|"
    { BAR }
| "?"
    { QUESTION }
| "*"
    { STAR }
| "+"
    { PLUS }
| "~"
    { TILDE }
| "_"
    { UNDERSCORE }
| ":="
    { COLONEQUAL }
| "=="
    { EQUALEQUAL }
| "let"
    { LET }
| (lowercase identchar *) as id
    { if Hashtbl.mem reserved id then
        error2 lexbuf "this is an OCaml reserved word."
      else
        LID (with_cpos lexbuf id)
    }
| (uppercase identchar *) as id
    { UID (with_cpos lexbuf id) }
(* Quoted strings are used as aliases for tokens. *)
(* A quoted string is stored as is -- with the quotes
   and with its escape sequences. *)
| '"'
    { let buffer = Buffer.create 16 in
      let openingpos = lexeme_start_p lexbuf in
      let content = record_string openingpos buffer lexbuf in
      let id = Printf.sprintf "\"%s\"" content in
      let pos = import (openingpos, lexbuf.lex_curr_p) in
      QID (with_loc pos id) }
| "//" [^ '\010' '\013']* newline (* skip C++ style comment *)
| newline
    { new_line lexbuf; main lexbuf }
| whitespace+
    { main lexbuf }
| "/*"
    { comment (lexeme_start_p lexbuf) lexbuf; main lexbuf }
| "(*"
    { ocamlcomment (lexeme_start_p lexbuf) lexbuf; main lexbuf }
| "<"
    { savestart lexbuf (ocamltype (lexeme_end_p lexbuf)) }
| "%{"
    { savestart lexbuf (fun lexbuf ->
        let openingpos = lexeme_start_p lexbuf in
        let stretchpos = lexeme_end_p lexbuf in
        let closingpos, monsters = action true openingpos [] lexbuf in
        no_monsters monsters;
        HEADER (mk_stretch stretchpos closingpos false [])
      ) }
| "{"
    { savestart lexbuf (fun lexbuf ->
        let openingpos = lexeme_start_p lexbuf in
        let stretchpos = lexeme_end_p lexbuf in
        let closingpos, monsters = action false openingpos [] lexbuf in
        ACTION (
          fun (*dollars*) producers ->
            (* Check that the monsters are well-formed. *)
            List.iter (fun monster -> monster.check (*dollars*) producers) monsters;
            (* Gather all of the identifiers that the semantic action may use
               to refer to a semantic value. This includes the identifiers
               that are explicitly bound by the user (these appear in the
               array [producers]) and the identifiers [_i] when the semantic
               action uses [$i]. *)
            let _ids =
              StringSet.union (gather_oids producers) (gather_monsters monsters)
            in
            (* Extract a stretch of text. *)
            let _stretch = mk_stretch stretchpos closingpos true monsters in
            (* Build a semantic action. *)
            failwith "TODO"
            (*Action.from_stretch ids stretch*)
        )
      )
    }
| ('%'? as percent) "[@" (attributechar+ as id) whitespace*
    { let openingpos = lexeme_start_p lexbuf in
      let stretchpos = lexeme_end_p lexbuf in
      let closingpos = attribute openingpos lexbuf in
      let pos = Positions.import (openingpos, lexeme_end_p lexbuf) in
      let attr = mk_stretch stretchpos closingpos false [] in
      if percent = "" then
        (* No [%] sign: this is a normal attribute. *)
        ATTRIBUTE (Positions.with_loc pos id, attr)
      else
        (* A [%] sign is present: this is a grammar-wide attribute. *)
        GRAMMARATTRIBUTE (Positions.with_loc pos id, attr)
    }
| eof
    { EOF }
| _
    { error2 lexbuf "unexpected character(s)." }

(* ------------------------------------------------------------------------ *)

(* Skip C style comments. *)

and comment openingpos = parse
| newline
    { new_line lexbuf; comment openingpos lexbuf }
| "*/"
    { () }
| eof
    { error1 openingpos "unterminated comment." }
| _
    { comment openingpos lexbuf }

(* ------------------------------------------------------------------------ *)

(* Collect an O'Caml type delimited by angle brackets. Angle brackets can
   appear as part of O'Caml function types and variant types, so we must
   recognize them and *not* treat them as a closing bracket. *)

and ocamltype openingpos = parse
| "->"
| "[>"
    { ocamltype openingpos lexbuf }
| '>'
    { OCAMLTYPE (mk_stretch openingpos (lexeme_start_p lexbuf) true []) }
| "(*"
    { ocamlcomment (lexeme_start_p lexbuf) lexbuf; ocamltype openingpos lexbuf }
| newline
    { new_line lexbuf; ocamltype openingpos lexbuf }
| eof
    { error1 openingpos "unterminated OCaml type." }
| _
    { ocamltype openingpos lexbuf }

(* ------------------------------------------------------------------------ *)

(* Collect O'Caml code delimited by curly brackets. The monsters that are
   encountered along the way are accumulated in the list [monsters]. Nested
   curly brackets must be properly counted. Nested parentheses are also kept
   track of, so as to better report errors when they are not balanced. *)

and action percent openingpos monsters = parse
| '{'
    { let _, monsters = action false (lexeme_start_p lexbuf) monsters lexbuf in
      action percent openingpos monsters lexbuf }
| ("}" | "%}") as delimiter
    { match percent, delimiter with
      | true, "%}"
      | false, "}" ->
          (* This is the delimiter we were instructed to look for. *)
          lexeme_start_p lexbuf, monsters
      | _, _ ->
          (* This is not it. *)
          error1 openingpos "unbalanced opening brace."
    }
| '('
    { let _, monsters = parentheses (lexeme_start_p lexbuf) monsters lexbuf in
      action percent openingpos monsters lexbuf }
| '$' (['0'-'9']+ as i)
    { let i = int_of_string (lexeme_start_p lexbuf) i in
      let monster = dollar (cpos lexbuf) i in
      action percent openingpos (monster :: monsters) lexbuf }
| poskeyword
    { let monster = position (cpos lexbuf) where flavor i x in
      action percent openingpos (monster :: monsters) lexbuf }
| previouserror
    { error2 lexbuf "$previouserror is no longer supported." }
| syntaxerror
    { error2 lexbuf "$syntaxerror is no longer supported." }
| '"'
    { string (lexeme_start_p lexbuf) lexbuf;
      action percent openingpos monsters lexbuf }
| "'"
    { char lexbuf;
      action percent openingpos monsters lexbuf }
| "(*"
    { ocamlcomment (lexeme_start_p lexbuf) lexbuf;
      action percent openingpos monsters lexbuf }
| newline
    { new_line lexbuf;
      action percent openingpos monsters lexbuf }
| ')'
| eof
    { error1 openingpos "unbalanced opening brace." }
| _
    { action percent openingpos monsters lexbuf }

(* ------------------------------------------------------------------------ *)

(* Inside a semantic action, we keep track of nested parentheses, so as to
   better report errors when they are not balanced. *)

and parentheses openingpos monsters = parse
| '('
    { let _, monsters = parentheses (lexeme_start_p lexbuf) monsters lexbuf in
      parentheses openingpos monsters lexbuf }
| ')'
    { lexeme_start_p lexbuf, monsters }
| '{'
    { let _, monsters = action false (lexeme_start_p lexbuf) monsters lexbuf in
      parentheses openingpos monsters lexbuf }
| '$' (['0'-'9']+ as i)
    { let i = int_of_string (lexeme_start_p lexbuf) i in
      let monster = dollar (cpos lexbuf) i in
      parentheses openingpos (monster :: monsters) lexbuf }
| poskeyword
    { let monster = position (cpos lexbuf) where flavor i x in
      parentheses openingpos (monster :: monsters) lexbuf }
| previouserror
    { error2 lexbuf "$previouserror is no longer supported." }
| syntaxerror
    { error2 lexbuf "$syntaxerror is no longer supported." }
| '"'
    { string (lexeme_start_p lexbuf) lexbuf; parentheses openingpos monsters lexbuf }
| "'"
    { char lexbuf; parentheses openingpos monsters lexbuf }
| "(*"
    { ocamlcomment (lexeme_start_p lexbuf) lexbuf; parentheses openingpos monsters lexbuf }
| newline
    { new_line lexbuf; parentheses openingpos monsters lexbuf }
| '}'
| eof
    { error1 openingpos "unbalanced opening parenthesis." }
| _
    { parentheses openingpos monsters lexbuf }

(* ------------------------------------------------------------------------ *)

(* Collect an attribute payload, which is terminated by a closing square
   bracket. Nested square brackets must be properly counted. Nested curly
   brackets and nested parentheses are also kept track of, so as to better
   report errors when they are not balanced. *)

and attribute openingpos = parse
| '['
    { let _ = attribute (lexeme_start_p lexbuf) lexbuf in
      attribute openingpos lexbuf }
| ']'
    { lexeme_start_p lexbuf }
| '{'
    { let _, _ = action false (lexeme_start_p lexbuf) [] lexbuf in
      attribute openingpos lexbuf }
| '('
    { let _, _ = parentheses (lexeme_start_p lexbuf) [] lexbuf in
      attribute openingpos lexbuf }
| '"'
    { string (lexeme_start_p lexbuf) lexbuf; attribute openingpos lexbuf }
| "'"
    { char lexbuf; attribute openingpos lexbuf }
| "(*"
    { ocamlcomment (lexeme_start_p lexbuf) lexbuf; attribute openingpos lexbuf }
| newline
    { new_line lexbuf; attribute openingpos lexbuf }
| '}'
| ')'
| eof
    { error1 openingpos "unbalanced opening bracket." }
| _
    { attribute openingpos lexbuf }

(* ------------------------------------------------------------------------ *)

(* Skip O'Caml comments. Comments can be nested and can contain
   strings or characters, which must be correctly analyzed. (A string
   could contain begin-of-comment or end-of-comment sequences, which
   must be ignored; a character could contain a begin-of-string
   sequence.) *)

and ocamlcomment openingpos = parse
| "*)"
    { () }
| "(*"
    { ocamlcomment (lexeme_start_p lexbuf) lexbuf; ocamlcomment openingpos lexbuf }
| '"'
    { string (lexeme_start_p lexbuf) lexbuf; ocamlcomment openingpos lexbuf }
| "'"
    { char lexbuf; ocamlcomment openingpos lexbuf }
| newline
    { new_line lexbuf; ocamlcomment openingpos lexbuf }
| eof
    { error1 openingpos "unterminated OCaml comment." }
| _
    { ocamlcomment openingpos lexbuf }

(* ------------------------------------------------------------------------ *)

(* Skip O'Caml strings. *)

and string openingpos = parse
| '"'
    { () }
| '\\' newline
| newline
    { new_line lexbuf; string openingpos lexbuf }
| '\\' _
    (* Upon finding a backslash, skip the character that follows,
       unless it is a newline. Pretty crude, but should work. *)
    { string openingpos lexbuf }
| eof
    { error1 openingpos "unterminated OCaml string." }
| _
    { string openingpos lexbuf }

(* ------------------------------------------------------------------------ *)

(* Recording on OCaml string. (This is used for token aliases.) *)

and record_string openingpos buffer = parse
| '"'
    { Buffer.contents buffer }
| ('\\' ['\\' '\'' '"' 't' 'b' 'r' ' ']) as sequence
    { (* This escape sequence is recognized as such, but not decoded. *)
      Buffer.add_string buffer sequence;
      record_string openingpos buffer lexbuf }
| '\\' 'n'
    (* We disallow this escape sequence in a token alias because we wish
       to use this string (unescaped) when we print a concrete sentence
       in a .messages file (see [Interpret]), and we want this sentence
       to fit on a single line. *)
    { error2 lexbuf "'\\n' is not permitted in a token alias." }
| '\\' _
    { error2 lexbuf "illegal backslash escape in string." }
| newline
    { error2 lexbuf "illegal newline in string." }
| eof
    { error1 openingpos "unterminated string." }
| _ as c
    { Buffer.add_char buffer c;
      record_string openingpos buffer lexbuf }

(* Decoding a string that may contain escaped characters. *)

and decode_string buffer = parse
| '"'
    { (* The final double quote is skipped. *) }
| '\\' (['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] as c)
    { Buffer.add_char buffer (char_for_backslash c);
      decode_string buffer lexbuf }
| _ as c
    { Buffer.add_char buffer c;
      decode_string buffer lexbuf }

(* ------------------------------------------------------------------------ *)

(* Skip O'Caml characters. A lone quote character is legal inside
   a comment, so if we don't recognize the matching closing quote,
   we simply abandon. *)

and char = parse
| '\\'? newline "'"
   { new_line lexbuf }
| [^ '\\' '\''] "'"
| '\\' _ "'"
| '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
| '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
| ""
   { () }

(* ------------------------------------------------------------------------ *)

(* Read until the end of the file. This is used after finding a %%
   that marks the end of the grammar specification. We update the
   current position as we go. This allows us to build a stretch
   for the postlude. *)

and finish = parse
| newline
    { new_line lexbuf; finish lexbuf }
| eof
    { lexeme_start_p lexbuf }
| _
    { finish lexbuf }
