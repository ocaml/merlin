(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module type S = sig
  type key
  type 'a t

  val empty : unit -> 'a t
  val bindings : 'a t -> (key * 'a) list
  val add : key -> 'a -> 'a t -> 'a t
  val singleton : key -> 'a -> 'a t
  val remove : key -> 'a t -> 'a t
  val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val cardinal : 'a t -> int
  val find : key -> 'a t -> 'a
  val find_opt : key -> 'a t -> 'a option
  val choose_opt : 'a t -> (key * 'a) option
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
  val map : ('a -> 'b) -> 'a t -> 'b t
  val is_empty : 'a t -> bool
  val mem : key -> 'a t -> bool
  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  val schema :
    'a t Type.Id.t ->
    Granular_marshal.iter ->
    (Granular_marshal.iter -> key -> 'a -> unit) ->
    'a t ->
    unit
end

module Make (Ord : Map.OrderedType) : S with type key = Ord.t
