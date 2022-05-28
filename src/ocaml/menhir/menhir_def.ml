(******************************************************************************)
(*                                                                            *)
(* Code is taken from Menhir, original copyright is:                          *)
(*                                                                            *)
(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

module StringSet = Set.Make(String)

module StringMap = Map.Make(String)

module Keyword = struct
  (* This module provides some type and function definitions
     that help deal with the keywords that we recognize within
     semantic actions. *)

  (* ------------------------------------------------------------------------- *)
  (* Types. *)

  (* The user can request position information either at type
     [int] (a simple offset) or at type [Lexing.position]. *)

  type flavor =
    | FlavorOffset
    | FlavorPosition
    | FlavorLocation

  (* The user can request position information about the $start or $end
     of a symbol. Also, $symbolstart requests the computation of the
     start position of the first nonempty element in a production. *)

  type where =
  | WhereSymbolStart
  | WhereStart
  | WhereEnd

  (* The user can request position information about a production's
     left-hand side or about one of the symbols in its right-hand
     side, which he can refer to by position or by name. *)

  type subject =
    | Before
    | Left
    | RightNamed of string

  (* Keywords inside semantic actions. They allow access to semantic
     values or to position information. *)

  type keyword =
    | Position of subject * where * flavor

  (* ------------------------------------------------------------------------- *)
  (* These auxiliary functions help map a [Position] keyword to the
     name of the variable that the keyword is replaced with. *)

  let where = function
    | WhereSymbolStart ->
        "symbolstart"
    | WhereStart ->
        "start"
    | WhereEnd ->
        "end"

  let subject = function
    | Before ->
        "__0_"
    | Left ->
        ""
    | RightNamed id ->
        Printf.sprintf "_%s_" id

  let flavor = function
    | FlavorPosition ->
        "pos"
    | FlavorOffset ->
        "ofs"
    | FlavorLocation ->
        "loc"

  let posvar s w f =
    match w, f with
    | _, (FlavorOffset | FlavorPosition) ->
        Printf.sprintf "_%s%s%s" (where w) (flavor f) (subject s)
    | WhereSymbolStart, FlavorLocation ->
        "_sloc"
    | WhereStart, FlavorLocation ->
        Printf.sprintf "_loc%s" (subject s)
    | _ ->
        assert false

  (* ------------------------------------------------------------------------- *)
  (* Sets of keywords. *)

  module KeywordSet = struct

    include Set.Make (struct
      type t = keyword
      let compare = compare
    end)

    let map f keywords =
      fold (fun keyword accu ->
        add (f keyword) accu
      ) keywords empty

  end
end

module Stretch = struct
  (* A stretch is a fragment of a source file. It holds the file name,
     the line number, and the line count (that is, the length) of the
     fragment. These are used to generate line number directives when the
     fragment is copied to an output file. It also holds the textual
     content of the fragment, as a string. The [raw_content] field holds
     the text that was found in the source file, while the [content]
     field holds the same text after transformation by the lexer (which
     may substitute keywords, insert padding, insert parentheses, etc.).
     See [Lexer.mk_stretch] and its various call sites in [Lexer]. *)

  type t = {
      filename    : string;
      linenum     : int;
      linecount   : int;
      raw_content : string;
      content     : string;
      keywords    : Keyword.keyword list;
    }
end

module Positions = struct
  open Lexing

  type t = Location.t = {
    loc_start: position;
    loc_end: position;
    loc_ghost: bool;
  }

  type 'a located = 'a Location.loc = {
    txt: 'a;
    loc: t;
  }

  let value l = l.txt

  let position l = l.loc

  let decompose {txt; loc} = (txt, loc)

  let with_loc loc txt = {txt; loc}
  (* The location is converted from the type [position * position]
       to the type [t]. *)

  let with_pos (loc_start, loc_end) txt =
    let loc = {loc_start; loc_end; loc_ghost=false} in
    {txt; loc}

  let map f v = {txt = f v.txt; loc = v.loc}

  let pmap f v = {txt = f v.loc v.txt; loc = v.loc}

  let iter f l = f l.txt

  let mapd f {txt; loc} =
    let t1, t2 = f txt in
    ({txt = t1; loc}, {txt = t2; loc})

  let dummy = Location.none

  let unknown_pos txt = {txt; loc = dummy}

  let start_of_position t = t.loc_start

  let end_of_position t = t.loc_end

  let filename_of_position p =
    (start_of_position p).pos_fname

  let line p =
    p.pos_lnum

  let column p =
    p.pos_cnum - p.pos_bol

  let characters p1 p2 =
    (column p1, p2.pos_cnum - p1.pos_bol) (* intentionally [p1.pos_bol] *)

  let join x1 x2 = {
    loc_start = start_of_position (if x1 = dummy then x2 else x1);
    loc_end = end_of_position (if x2 = dummy then x1 else x2);
    loc_ghost = false;
  }

  let import (x, y) = {loc_start = x; loc_end = y; loc_ghost = false}

  let join_located l1 l2 f = {
    txt = f l1.txt l2.txt;
    loc = join l1.loc l2.loc;
  }

  let string_of_lex_pos p =
    let c = p.pos_cnum - p.pos_bol in
    (string_of_int p.pos_lnum ^ ":" ^ string_of_int c)

  let string_of_pos p =
    let filename = filename_of_position p in
    (* [filename] is hopefully not "". *)
    let l = line (start_of_position p) in
    let c1, c2 = characters (start_of_position p) (end_of_position p) in
    Printf.sprintf "File \"%s\", line %d, characters %d-%d" filename l c1 c2

  let pos_or_undef = function
    | None -> dummy
    | Some x -> x

  let cpos lexbuf = {
    loc_start = lexeme_start_p lexbuf;
    loc_end = lexeme_end_p lexbuf;
    loc_ghost = false;
  }

  let with_cpos lexbuf v =
    with_loc (cpos lexbuf) v

  let string_of_cpos lexbuf =
    string_of_pos (cpos lexbuf)

  let joinf f t1 t2 =
    join (f t1) (f t2)

  let ljoinf f =
    List.fold_left (fun p t -> join p (f t)) dummy

  let join_located_list ls f = {
    txt = f (List.map (fun l -> l.txt) ls);
    loc = ljoinf (fun x -> x.loc) ls
  }

  (* The functions that print error messages and warnings require a list of
     positions. The following auxiliary functions help build such lists. *)

  type positions = t list

  let one (pos : position) : positions =
    [ import (pos, pos) ]

  let lexbuf (lexbuf : lexbuf) : positions =
    [ import (lexbuf.lex_start_p, lexbuf.lex_curr_p) ]

  let print (pos : position) =
    Printf.printf
      "{ pos_fname = \"%s\"; pos_lnum = %d; pos_bol = %d; pos_cnum = %d }\n"
        pos.pos_fname pos.pos_lnum pos.pos_bol pos.pos_cnum
end

module Error = struct
  (* ---------------------------------------------------------------------------- *)

  (* A mechanism to turn all display (logging, warnings, errors) on and off. *)

  let enabled =
    ref true

  let enable () =
    enabled := true

  let disable () =
    enabled := false

  (* ---------------------------------------------------------------------------- *)

  (* Errors and warnings. *)

  let print_positions f positions =
    List.iter (fun position ->
      Printf.fprintf f "%s:\n" (Positions.string_of_pos position)
    ) positions

  let display continuation header positions format =
    let kprintf = if !enabled then Printf.kfprintf else Printf.ikfprintf in
    kprintf continuation stderr
      ("%a" ^^ header ^^ format ^^ "\n%!")
      print_positions positions

  let error positions format =
    display
      (fun _ -> exit 1)
      "Error: "
      positions format

  let warning positions format =
    display
      (fun _ -> ())
      "Warning: "
      positions format

  let errorp v =
    error [ Positions.position v ]

  (* ---------------------------------------------------------------------------- *)

  (* Delayed error reports -- where multiple errors can be reported at once. *)

  type category =
    bool ref

  let new_category () =
    ref false

  let signal category positions format =
    display
      (fun _ -> category := true)
      "Error: "
      positions format

  let exit_if category =
    if !category then
      exit 1

  let with_new_category f =
    let c = new_category() in
    match f c with
    | y ->
        exit_if c;
        y
    | exception e ->
        exit_if c;
        raise e

  (* ---------------------------------------------------------------------------- *)

  (* Certain warnings about the grammar can optionally be treated as errors. *)

  let grammatical_error = new_category()
end

module InputFile = struct
  type input_file
  let chunk _ = ""
  let get_input_file_name () = ""
end
