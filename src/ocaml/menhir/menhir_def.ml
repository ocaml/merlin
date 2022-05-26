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
      stretch_filename    : string;
      stretch_linenum     : int;
      stretch_linecount   : int;
      stretch_raw_content : string;
      stretch_content     : string;
      stretch_keywords    : Keyword.keyword list
    }

  (* An OCaml type is either a stretch (if it was found in some
     source file) or a string (if it was inferred via [Infer]). *)

  type ocamltype =
    | Declared of t
    | Inferred of string
end

module Positions = struct
  open Lexing

  type t =
    (* Start and end positions. *)
    position * position

  type 'a located = {
    value    : 'a;
    position : t;
  }

  let value l = l.value

  let position l = l.position

  let decompose { value; position } =
    (value, position)

  let with_pos p v =
    {
      value     = v;
      position  = p;
    }

  let with_loc =
    (* The location is converted from the type [position * position]
       to the type [t]. *)
    with_pos

  let map f v =
    {
      value     = f v.value;
      position  = v.position;
    }

  let pmap f v =
    {
      value     = f v.position v.value;
      position  = v.position
    }

  let iter f l = f l.value

  let mapd f v =
    let w1, w2 = f v.value in
    let pos = v.position in
    { value = w1; position = pos },
    { value = w2; position = pos }

  let dummy =
    (dummy_pos, dummy_pos)

  let unknown_pos v =
    {
      value     = v;
      position  = dummy
    }

  let start_of_position (p, _) = p

  let end_of_position (_, p) = p

  let filename_of_position p =
    (start_of_position p).pos_fname

  let line p =
    p.pos_lnum

  let column p =
    p.pos_cnum - p.pos_bol

  let characters p1 p2 =
    (column p1, p2.pos_cnum - p1.pos_bol) (* intentionally [p1.pos_bol] *)

  let join x1 x2 =
  (
    start_of_position (if x1 = dummy then x2 else x1),
    end_of_position   (if x2 = dummy then x1 else x2)
  )

  let import x =
    x

  let join_located l1 l2 f =
    {
      value    = f l1.value l2.value;
      position = join l1.position l2.position;
    }

  let string_of_lex_pos p =
    let c = p.pos_cnum - p.pos_bol in
    (string_of_int p.pos_lnum)^":"^(string_of_int c)

  let string_of_pos p =
    let filename = filename_of_position p in
    (* [filename] is hopefully not "". *)
    let l = line (start_of_position p) in
    let c1, c2 = characters (start_of_position p) (end_of_position p) in
    Printf.sprintf "File \"%s\", line %d, characters %d-%d" filename l c1 c2

  let pos_or_undef = function
    | None -> dummy
    | Some x -> x

  let cpos lexbuf =
    (lexeme_start_p lexbuf, lexeme_end_p lexbuf)

  let with_cpos lexbuf v =
    with_pos (cpos lexbuf) v

  let string_of_cpos lexbuf =
    string_of_pos (cpos lexbuf)

  let joinf f t1 t2 =
    join (f t1) (f t2)

  let ljoinf f =
    List.fold_left (fun p t -> join p (f t)) dummy

  let join_located_list ls f =
    {
      value     = f (List.map (fun l -> l.value) ls);
      position  = ljoinf (fun x -> x.position) ls
    }

  (* The functions that print error messages and warnings require a list of
     positions. The following auxiliary functions help build such lists. *)

  type positions =
      t list

  let one (pos : position) : positions =
    [ import (pos, pos) ]

  let lexbuf (lexbuf : lexbuf) : positions =
    [ import (lexbuf.lex_start_p, lexbuf.lex_curr_p) ]

  let print (pos : position) =
    Printf.printf
      "{ pos_fname = \"%s\"; pos_lnum = %d; pos_bol = %d; pos_cnum = %d }\n"
        pos.pos_fname
        pos.pos_lnum
        pos.pos_bol
        pos.pos_cnum
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

  let grammatical_error =
    new_category()
end

module InputFile = struct
  type input_file
  let chunk _ = ""
  let get_input_file_name () = ""
end
