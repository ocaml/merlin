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

(* System configuration *)

val version: string
        (* The current version number of the system *)

val ext_obj : string

val interface_suffix: string ref
        (* Suffix for interface file names *)

val exec_magic_number: string
        (* Magic number for bytecode executable files *)
val cmi_magic_number: string
        (* Magic number for compiled interface files *)
val cmo_magic_number: string
        (* Magic number for object bytecode files *)
val cma_magic_number: string
        (* Magic number for archive files *)
val cmx_magic_number: string
        (* Magic number for compilation unit descriptions *)
val cmxa_magic_number: string
        (* Magic number for libraries of compilation unit descriptions *)
val ast_intf_magic_number: string
        (* Magic number for file holding an interface syntax tree *)
val ast_impl_magic_number: string
        (* Magic number for file holding an implementation syntax tree *)
val cmxs_magic_number: string
        (* Magic number for dynamically-loadable plugins *)
val cmt_magic_number: string
        (* Magic number for compiled interface files *)
val index_magic_number: string
        (* Magic number for index files *)


val max_tag: int
        (* Biggest tag that can be stored in the header of a regular block. *)

val flat_float_array : bool
        (* Whether the compiler and runtime automagically flatten float arrays *)

(**/**)

val merlin : bool


(**/**)
