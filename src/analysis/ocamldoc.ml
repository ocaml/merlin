(**************************************************************************)
(*                                                                        *)
(*  Copyright 2013 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the Lesser GNU Public License version 3.0.                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  Lesser GNU General Public License for more details.                   *)
(*                                                                        *)
(**************************************************************************)

(** Pops comments from a list of comments (string * loc) to find the ones that
   are associated to a given location. Also returns the remaining comments after
   the location. [source] is the text of the document. *)
let associate_comment ~source ~after_only comments loc =
  let lstart = loc.Location.loc_start.Lexing.pos_lnum
  and lend = loc.Location.loc_end.Lexing.pos_lnum in
  (* A comment on a line after the item it documents must be the first thing on
     its line; otherwise it belongs to the code preceding it. If the source is not
     available, assume it is. *)
  let starts_its_line cloc =
    match source with
    | None -> true
    | Some source ->
      let start = cloc.Location.loc_start.pos_bol in
      let stop = min cloc.Location.loc_start.pos_cnum (String.length source) in
      let rec only_blanks_from i =
        if i >= stop then true
        else
          match source.[i] with
          | ' ' | '\t' -> only_blanks_from (i + 1)
          | _ -> false
      in
      only_blanks_from start
  in
  let rec aux = function
    | [] -> (None, [])
    | (comment, cloc) :: comments -> (
      let cstart = cloc.Location.loc_start.Lexing.pos_lnum
      and cend = cloc.Location.loc_end.Lexing.pos_lnum in
      let processed =
        (* It seems 4.02.3 remove ** from doc comment string, but not from
         * locations.  We can recognize doc comment by checking how the two
         * differ. *)
        cloc.Location.loc_end.Lexing.pos_cnum
        - cloc.Location.loc_start.Lexing.pos_cnum
        = String.length comment + 5
      in
      if cend < lstart - 1 || (cstart < lend && after_only) then aux comments
      else if
        cstart > lend + 1
        || (cstart > lstart && cend < lend (* keep inner comments *))
        || (cstart > lend && not (starts_its_line cloc))
      then (None, (comment, cloc) :: comments)
      else if
        String.length comment < 2
        || ((not processed) && (comment.[0] <> '*' || comment.[1] = '*'))
      then aux comments
      else
        let comment =
          if processed then comment
          else String.sub comment 1 (String.length comment - 1)
        in
        let comment = String.trim comment in
        match aux comments with
        | None, comments -> (Some comment, comments)
        | Some c, comments ->
          (Some (String.concat "\n" [ comment; c ]), comments))
  in
  aux comments
