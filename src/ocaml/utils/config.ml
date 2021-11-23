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

(***********************************************************************)
(**                                                                   **)
(**               WARNING WARNING WARNING                             **)
(**                                                                   **)
(** When you change this file, you must make the parallel change      **)
(** in config.mlbuild                                                 **)
(**                                                                   **)
(***********************************************************************)


(* The main OCaml version string has moved to ../VERSION *)
let version = Sys.ocaml_version

let flambda = false

let exec_magic_number = "Caml1999X030"
    (* exec_magic_number is duplicated in runtime/caml/exec.h *)
and cmi_magic_number = "Caml1999I030"
and cmo_magic_number = "Caml1999O030"
and cma_magic_number = "Caml1999A030"
and cmx_magic_number =
  if flambda then
    "Caml1999y030"
  else
    "Caml1999Y030"
and cmxa_magic_number =
  if flambda then
    "Caml1999z030"
  else
    "Caml1999Z030"
and ast_impl_magic_number = "Caml1999M030"
and ast_intf_magic_number = "Caml1999N030"
and cmxs_magic_number = "Caml1999D030"
and cmt_magic_number = "Caml1999T030"
and linear_magic_number = "Caml1999L030"

let interface_suffix = ref ".mli"

let max_tag = 245

let safe_string = true
let flat_float_array = false

let merlin = true
