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
open Location

type t =
    Lident of string
  | Ldot of t loc * string loc
  | Lapply of t loc * t loc


let rec same t t' =
  t == t'
  || match t, t' with
  | Lident s, Lident s' ->
      String.equal s s'
  | Ldot ({ txt = t; _ }, { txt = s; _ }),
    Ldot ({ txt = t'; _ }, { txt = s'; _ }) ->
      if String.equal s s' then
        same t t'
      else
        false
  | Lapply ({ txt = tl; _ }, { txt = tr; _ }),
    Lapply ({ txt = tl'; _ }, { txt = tr'; _ }) ->
      same tl tl' && same tr tr'
  | _, _ -> false


let rec flat accu = function
    Lident s -> s :: accu
  | Ldot({ txt = lid; _ }, { txt = s; _ }) -> flat (s :: accu) lid
  | Lapply(_, _) -> Misc.fatal_error "Longident.flat"

let flatten lid = flat [] lid

let rec head = function
    Lident s -> s
  | Ldot(lid, _) -> head lid.txt
  | Lapply(_, _) -> assert false

let last = function
    Lident s -> s
  | Ldot(_, s) -> s.txt
  | Lapply(_, _) -> Misc.fatal_error "Longident.last"


let rec split_at_dots s pos =
  try
    let dot = String.index_from s pos '.' in
    String.sub s pos (dot - pos) :: split_at_dots s (dot + 1)
  with Not_found ->
    [String.sub s pos (String.length s - pos)]

let unflatten l =
  match l with
  | [] -> None
  | hd :: tl ->
    Some (List.fold_left (fun p s -> Ldot(mknoloc p, mknoloc s))
                         (Lident hd) tl)

let parse s =
  match unflatten (split_at_dots s 0) with
  | None -> Lident ""  (* should not happen, but don't put assert false
                          so as not to crash the toplevel (see Genprintval) *)
  | Some v -> v

let keep_suffix =
  let rec aux = function
    | { txt = Lident str; _ } as t ->
      if String.uncapitalize_ascii str <> str then
        Some (t, false)
      else
        None
    | { txt = Ldot (t, str); loc } ->
      if String.uncapitalize_ascii str.txt <> str.txt then
        match aux t with
        | None -> Some ({ txt = Lident str.txt; loc = str.loc }, true)
        | Some (t, is_label) -> Some ({ txt = Ldot (t, str); loc }, is_label)
      else
        None
    | t -> Some (t, false) (* Can be improved... *)
  in
  function
  | Lident s -> Lident s, false
  | Ldot (t, s) ->
    begin match aux t with
    | None -> Lident s.txt, true
    | Some (t, is_label) -> Ldot (t, s), is_label
    end
  | otherwise -> otherwise, false
