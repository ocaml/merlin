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

let ext_obj = ".o_The boot compiler cannot process C objects"

let exec_magic_number = "Caml1999X035"
    (* exec_magic_number is duplicated in runtime/caml/exec.h *)
and cmi_magic_number = "Caml1999I035"
and cmo_magic_number = "Caml1999O035"
and cma_magic_number = "Caml1999A035"
and cmx_magic_number =
  if flambda then
    "Caml1999y035"
  else
    "Caml1999Y035"
and cmxa_magic_number =
  if flambda then
    "Caml1999z035"
  else
    "Caml1999Z035"
and ast_impl_magic_number = "Caml1999M035"
and ast_intf_magic_number = "Caml1999N035"
and cmxs_magic_number = "Caml1999D035"
and cmt_magic_number = "Caml1999T035"
and index_magic_number = "Merl2023I003"

let interface_suffix = ref ".mli"
let flat_float_array = true

let max_tag = 245

let merlin = true
