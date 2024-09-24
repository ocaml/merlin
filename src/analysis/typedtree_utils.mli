(** Utilities to keep explicit Typedtree manipulations local to Merlin_lib
    for alternative clients like [ocaml-lsp]. *)

(** [extract_toplevel_identifier sigitem] extracts toplevel identifier of
    a signature item. It returns a list for dealing with recursive elements. *)
val extract_toplevel_identifier : Typedtree.signature_item -> Ident.t list

(** [let_bound_vars binding_list] extract the [Ident.t] and the
    location of variables bind in the form of [let b = e ...] in a
    list of bindings. *)
val let_bound_vars :
  Typedtree.value_binding list -> (Ident.t * string Location.loc) list

(** Extracts the location of a [uid] from a
    [Typedtree.item_declaration] *)
val location_of_declaration :
  uid:Shape.Uid.t -> Typedtree.item_declaration -> string Location.loc option

(** [pat_var_id_and_loc] try to extract the [id] and the [location] of
    pattern variable. *)
val pat_var_id_and_loc :
  Typedtree.pattern -> (Ident.t * string Location.loc) option

(** [pat_alias_id_and_loc] try to extract the [id] and the [location]
    of pattern alias. *)
val pat_alias_pat_id_and_loc :
  Typedtree.pattern ->
  (Typedtree.pattern * Ident.t * string Location.loc) option
