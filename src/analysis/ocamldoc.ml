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

let associate_comments ~source ~after_only comments (loc : Location.t) =
  (* https://ocaml.org/manual/5.5/doccomments.html#s:doc-comments *)
  let lstart = loc.loc_start.pos_lnum in
  let lend = loc.loc_end.pos_lnum in
  (* A comment on a line after the item it documents must be the first thing on
     its line; otherwise it belongs to the code preceding it. If the source is not
     available, assume it is. *)
  let starts_its_line (cloc : Location.t) =
    match source with
    | None -> true
    | Some source ->
      let start = cloc.loc_start.pos_bol in
      let stop = min cloc.loc_start.pos_cnum (String.length source) in
      let rec only_blanks_from i =
        if i >= stop then true
        else
          match source.[i] with
          | ' ' | '\t' -> only_blanks_from (i + 1)
          | _ -> false
      in
      only_blanks_from start
  in
  let associated_comments =
    List.filter_map
      (fun (comment, (cloc : Location.t)) ->
        let cstart = cloc.loc_start.pos_lnum in
        let cend = cloc.loc_end.pos_lnum in
        let processed =
          (* It seems 4.02.3 remove ** from doc comment string, but not from
           * locations.  We can recognize doc comment by checking how the two
           * differ. *)
          cloc.loc_end.pos_cnum - cloc.loc_start.pos_cnum
          = String.length comment + 5
        in
        if
          cend < lstart - 1
          (* comment is too far before the decl *)
          || cstart > lend + 1
          (* comment is too far after the decl *)
          || (cstart < lend && after_only)
             (* comment is before the decl, but we only want ones after *)
          || (cstart > lstart && cend < lend (* comment is within decl *))
          || (cstart > lend && not (starts_its_line cloc))
             (* comment is after the decl, and there's another decl between the decl and the comment *)
          || String.length comment < 2
          || ((not processed) && (comment.[0] <> '*' || comment.[1] = '*'))
        then None
        else
          let comment =
            if processed then comment
            else String.sub comment 1 (String.length comment - 1)
          in
          let comment = String.trim comment in
          Some comment)
      comments
  in
  match associated_comments with
  | [] -> None
  | associated_comments -> Some (String.concat "\n" associated_comments)
