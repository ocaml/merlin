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

(* Helpers for TypeWith *)
type type_scheme = [
  | `Var   of string
  | `Arrow of Asttypes.label * type_scheme * type_scheme
  | `Named of type_scheme list * string
]

(* extend as needed *)
type ast = [
  | `Let   of binding list
  | `Fun   of string list * ast
  | `App   of ast * ast
  | `Ident of string
  | `AnyVal (* wild card ident *)
  | `Val   of string * type_scheme (* TODO: use something similar to [binding] type? *)
]
and binding = {
  ident   : string ;
  typesig : type_scheme ;
  body    : ast ;
}

module Sexp : sig
  type ty = string Location.loc * Parsetree.type_declaration
  module Struct : sig
    val make_funs : ty -> [ `Let of binding list ]
  end
  module Sig : sig
    val make_decls : ty -> [ `Val of string * type_scheme ] list
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
