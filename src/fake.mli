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

(* Helpers; extend as needed *)
module Ast : sig
  type type_scheme = [
    | `Var   of string
    | `Arrow of Asttypes.label * type_scheme * type_scheme
    | `Named of type_scheme list * string
  ]

  type str_item = [
    | `Let of expr binding
  ]
  and sig_item = [
    | `Val of unit binding
  ]
  and expr = [
    | `Fun   of string list * expr
    | `App   of expr * expr
    | `Ident of string
    | `AnyVal (* wild card ident *)
  ]
  and 'a binding = {
    ident   : string ;
    typesig : type_scheme ;
    body    : 'a ;
  }
end

module Sexp : sig
  type ty = string Location.loc * Parsetree.type_declaration
  module Struct : sig
    val make_funs : ty -> Ast.str_item list
  end
  module Sig : sig
    val make_decls : ty -> Ast.sig_item list
  end
end

(* type-conv extension *)
module TypeWith : sig
  (* Simulate behavior of type-conv generators. Supported generators are:
   * - sexp,
   * - bin_io, bin_read, bin_write.  *)
  type generator = string

  val generate_definitions : ty:Sexp.ty list -> ?ghost_loc:Location.t ->
    generator list -> Parsetree.structure_item list
  val generate_sigs : ty:Sexp.ty list -> ?ghost_loc:Location.t ->
    generator list -> Parsetree.signature_item list
end
