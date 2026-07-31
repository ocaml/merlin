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

(** [associate_comments ~source ~after_only comments loc] finds the doc comments in
    [comments] associated with the declaration at [loc]. [source] is the text of the
    document, which is used to help associate comments correctly. If [source] is [None],
    some comments may be incorrectly associated. *)
val associate_comments :
  source:string option ->
  after_only:bool ->
  (string * Location.t) list ->
  Location.t ->
  string option
