(** Utilities to keep explicit Typedtree manipulations local to Merlin_lib
    for alternative clients like [ocaml-lsp]. *)

(** [extract_toplevel_identifier sigitem] extracts toplevel identifier of
    a signature item. It returns a list for dealing with recursive elements. *)
val extract_toplevel_identifier : Typedtree.signature_item -> Ident.t list

val let_bound_vars :
  Typedtree.value_binding list -> (Ident.t * string Location.loc) list

