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
