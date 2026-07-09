(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Liam Stevenson, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
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
