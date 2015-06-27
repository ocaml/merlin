(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2014  Frédéric Bour  <frederic.bour(_)lakaban.net>
                             Thomas Refis  <refis.thomas(_)gmail.com>
                             Simon Castellan  <simon.castellan(_)iuwt.fr>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

(* Definitions to help generating or rewriting pieces of AST,
 * used to simulate some CamlP4 extensions. *)

(* Generate AST faking value application *)
val app : Parsetree.expression ->
  Parsetree.expression -> Parsetree.expression
val pat_app : Parsetree.expression ->
  ('a * Parsetree.expression) -> ('a * Parsetree.expression )

(* Bottom value (e.g. forall 'a. 'a) used to substitute
 * syntactically incorrect expressions during error-recovery.  *)
val any_val' : Parsetree.expression

(* Lwt extension *)
module Lwt : sig
  val un_lwt     : Parsetree.expression
  val to_lwt     : Parsetree.expression
  val in_lwt     : Parsetree.expression
  val unit_lwt   : Parsetree.expression
  val un_stream  : Parsetree.expression
  val finally'   : Parsetree.expression
  val raise_lwt' : Longident.t
end

(* Js extension *)
module Js : sig
  val un_js     : Parsetree.expression
  val un_meth   : Parsetree.expression
  val un_constr : Parsetree.expression
  val un_prop   : Parsetree.expression
end

(* OUnit extension *)
module OUnit : sig
  val fresh_test_module_ident : unit -> string
  val force_bool : Parsetree.expression
  val force_unit : Parsetree.expression
  val force_unit_arrow_unit : Parsetree.expression
  val force_indexed : Parsetree.expression
end

type tydecl = string Location.loc * Parsetree.type_declaration

(* type-conv extension *)
module TypeWith : sig
  (* Simulate behavior of type-conv generators. Supported generators are:
   * - sexp,
   * - bin_io, bin_read, bin_write.  *)
  type generator = string

  val generate_definitions : ty:tydecl list -> ?ghost_loc:Location.t ->
    generator list -> Parsetree.structure_item list
  val generate_sigs : ty:tydecl list -> ?ghost_loc:Location.t ->
    generator list -> Parsetree.signature_item list
end

module Nonrec : sig
  val add  : string Location.loc -> string Location.loc
  val is   : string -> bool
  val drop : string -> string
  val drop_loc : string Location.loc -> string Location.loc
end

(* Custom printf extension *)
module Custom_printf : sig
  val bang : Lexing.position -> Lexing.position ->
    Parsetree.expression -> Parsetree.expression option
end

(* MetaOCaml support *)
module Meta : sig
  val code : Lexing.position -> Lexing.position ->
    Parsetree.expression -> Parsetree.expression
  val uncode : Lexing.position -> Lexing.position ->
    Parsetree.expression -> Parsetree.expression
end
