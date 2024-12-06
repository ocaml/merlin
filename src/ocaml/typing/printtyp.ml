(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*  Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Printing functions *)

module Fmt = Format_doc
module Doc = Printtyp_doc

include Doc

(* Print a long identifier *)
let longident = Pprintast.longident
let ident = Fmt.compat Doc.ident
let path = Fmt.compat Doc.path
let type_path = Fmt.compat Doc.type_path
let raw_type_expr = Fmt.compat Doc.raw_type_expr
let wrap_printing_env = Doc.wrap_printing_env
let type_expr = Fmt.compat Doc.type_expr
let prepared_type_expr = Fmt.compat Doc.prepared_type_expr
let constructor_arguments = Fmt.compat Doc.constructor_arguments
let type_scheme = Fmt.compat Doc.type_scheme
let prepared_type_scheme = Fmt.compat Doc.prepared_type_scheme
let shared_type_scheme = Fmt.compat Doc.shared_type_scheme
let value_description = Fmt.compat1 Doc.value_description
let label = Fmt.compat Doc.label
let prepared_constructor = Fmt.compat Doc.prepared_constructor
let constructor = Fmt.compat Doc.constructor
let prepared_type_declaration = Fmt.compat1 Doc.prepared_type_declaration
let type_declaration = Fmt.compat1 Doc.type_declaration

let prepared_extension_constructor =
  Fmt.compat1 Doc.prepared_extension_constructor

let extension_constructor = Fmt.compat1 Doc.extension_constructor
let extension_only_constructor = Fmt.compat1 Doc.extension_only_constructor
let modtype = Fmt.compat Doc.modtype
let signature = Fmt.compat Doc.signature

let rec functor_parameters ~sep custom_printer = function
  | [] -> ignore
  | [id,param] ->
    Format.dprintf "%t%t"
      (custom_printer param)
      (functor_param ~sep ~custom_printer id [])
  | (id,param) :: q ->
    Format.dprintf "%t%a%t"
      (custom_printer param)
      sep ()
      (functor_param ~sep ~custom_printer id q)
and functor_param ~sep ~custom_printer id q =
  match id with
  | None -> functor_parameters ~sep custom_printer q
  | Some id ->
    Doc.Naming_context.with_arg id
      (fun () -> functor_parameters ~sep custom_printer q)

let modtype_declaration = Fmt.compat1 Doc.modtype_declaration
let class_type = Fmt.compat Doc.class_type
let class_declaration = Fmt.compat1 Doc.class_declaration
let cltype_declaration = Fmt.compat1 Doc.cltype_declaration
let type_expansion = Fmt.compat1 Doc.type_expansion
let printed_signature = Fmt.compat1 Doc.printed_signature


let () = Env.print_longident := Doc.longident
let () = Env.print_path := Doc.path
let () = Btype.print_raw := Doc.raw_type_expr
let () = Env.shorten_module_path := shorten_module_path
