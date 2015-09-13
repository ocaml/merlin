(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)
open Misc

(***********************************************************************)
(**                                                                   **)
(**               WARNING WARNING WARNING                             **)
(**                                                                   **)
(** When you change this file, you must make the parallel change      **)
(** in config.mlp                                                     **)
(**                                                                   **)
(***********************************************************************)


(* The main OCaml version string has moved to ../VERSION *)
let version = Sys.ocaml_version

let standard_library_default = Findlib.ocaml_stdlib ()

let standard_library =
  try
    Sys.getenv "OCAMLLIB"
  with Not_found ->
  try
    Sys.getenv "CAMLLIB"
  with Not_found ->
    standard_library_default

let windows =
  match Sys.os_type with
  | "Win32" -> true
  |    _    -> false

let sf = Printf.sprintf

let exec_magic_number = "Caml1999X008"
and cmi_magic_number = "Caml1999I015"
and cmo_magic_number = "Caml1999O007"
and cma_magic_number = "Caml1999A008"
and cmx_magic_number = "Caml1999Y011"
and cmxa_magic_number = "Caml1999Z010"
and ast_impl_magic_number = "Caml1999M016"
and ast_intf_magic_number = "Caml1999N015"
and cmxs_magic_number = "Caml2007D001"
and cmt_magic_number = "Caml2012T002"

let load_path = ref []

let interface_suffix = ref ".mli"

let max_tag = 245

let print_config oc =
  let p name valu = Printf.fprintf oc "%s: %s\n" name valu in
  p "version" version;
  p "standard_library_default" standard_library_default;
  p "standard_library" standard_library;
  p "os_type" Sys.os_type;
  flush oc;
;;
