open Browse_raw

type syntax_info = Query_protocol.syntax_doc_result option

let syntax_doc_url endpoint =
  let base_url = "https://v2.ocaml.org/releases/4.14/htmlman/" in
  base_url ^ endpoint

let get_syntax_doc node : syntax_info =
  match node with
  | (_, Type_kind _)
    :: (_, Type_declaration _)
    :: (_, With_constraint (Twith_typesubst _))
    :: _ ->
      Some
        {
          name = "Destructive substitution";
          description =
            "Behaves like normal signature constraints but removes the \
             redefined type or module from the signature.";
          documentation =
            syntax_doc_url
              "signaturesubstitution.html#ss:destructive-substitution";
        }
  | (_, Type_kind _)
    :: (_, Type_declaration _)
    :: (_, Signature_item ({ sig_desc = Tsig_typesubst _; _ }, _))
    :: _ ->
      Some
        {
          name = "Local substitution";
          description =
            "Behaves like destructive substitution but is introduced during \
             the specification of the signature, and will apply to all the \
             items that follow.";
          documentation =
            syntax_doc_url "signaturesubstitution.html#ss:local-substitution";
        }
  | (_, Module_type _)
    :: (_, Module_type _)
    :: ( _,
         Module_type_constraint
           (Tmodtype_explicit
             { mty_desc = Tmty_with (_, [ (_, _, Twith_modtype _) ]); _ }) )
    :: _ ->
      Some
        {
          name = "Module substitution";
          description =
            "Behaves like type substitutions but are useful to refine an \
             abstract module type in a signature into a concrete module type,";
          documentation =
            syntax_doc_url
              "signaturesubstitution.html#ss:module-type-substitution";
        }
  | (_, Constructor_declaration _)
    :: (_, Type_kind (Ttype_variant _))
    :: (_, Type_declaration { typ_private = Public; _ })
    :: _
  | _
    :: (_, Constructor_declaration _)
    :: (_, Type_kind (Ttype_variant _))
    :: (_, Type_declaration { typ_private = Public; _ })
    :: _ ->
      Some
        {
          name = "Variant Type";
          description =
            "Represent data that may take on multiple different forms.";
          documentation = syntax_doc_url "typedecl.html#ss:typedefs";
        }
  | (_, Type_kind Ttype_open)
    :: (_, Type_declaration { typ_private = Public; _ })
    :: _ ->
      Some
        {
          name = "Extensible variant type";
          description =
            "Can be extended with new variant constructors using `+=`.";
          documentation = syntax_doc_url "extensiblevariants.html";
        }
  | (_, Type_kind Ttype_abstract)
    :: (_, Type_declaration { typ_private = Public; _ })
    :: _ ->
      Some
        {
          name = "Abstract type";
          description =
            "Define variants with arbitrary data structures, including other \
             variants, records, and functions";
          documentation = syntax_doc_url "typedecl.html#ss:typedefs";
        }
  | (_, Core_type _)
    :: (_, Core_type _)
    :: (_, Label_declaration _)
    :: (_, Type_kind (Ttype_record _))
    :: (_, Type_declaration { typ_private = Public; _ })
    :: _ ->
      Some
        {
          name = "Record type";
          description = "Define variants with a fixed set of fields";
          documentation = syntax_doc_url "typedecl.html#ss:typedefs";
        }
  | (_, Type_kind _) :: (_, Type_declaration { typ_private = Public; _ }) :: _
    ->
      Some
        {
          name = "Empty Variant type";
          description = "An empty variant type.";
          documentation = syntax_doc_url "emptyvariants.html";
        }
  | (_, Constructor_declaration _)
    :: (_, Type_kind (Ttype_variant _))
    :: (_, Type_declaration { typ_private = Private; _ })
    :: _
  | _
    :: (_, Constructor_declaration _)
    :: (_, Type_kind (Ttype_variant _))
    :: (_, Type_declaration { typ_private = Private; _ })
    :: _ ->
      Some
        {
          name = "Private Type";
          description =
            "Can be de-structured normally in pattern-matching but cannot be \
             constructed directly by constructor application.";
          documentation =
            syntax_doc_url "privatetypes.html#ss:private-types-variant";
        }
  | _ :: _
    :: (_, Label_declaration _)
    :: (_, Type_kind (Ttype_record _))
    :: (_, Type_declaration { typ_private = Private; _ })
    :: _ ->
      Some
        {
          name = "Private Record Type";
          description =
            "Can be de-structured normally in pattern-matching but cannot be \
             constructed directly by constructor application.";
          documentation =
            syntax_doc_url "privatetypes.html#ss:private-types-variant";
        }
  | (_, Type_kind Ttype_open)
    :: (_, Type_declaration { typ_private = Private; _ })
    :: _ ->
      Some
        {
          name = "Private Extensible Variant Type";
          description =
            "Prevents new constructors from being declared directly, but \
             allows extension constructors to be referred to in interfaces.";
          documentation =
            syntax_doc_url "extensiblevariants.html#ss:private-extensible";
        }
  | (_, Type_kind Ttype_abstract)
    :: (_, Type_declaration { typ_private = Private; _ })
    :: _ ->
      Some
        {
          name = "Private Type Abbreviation";
          description =
            "Declares a type that is distinct from its implementation type \
             `typexpr`.";
          documentation =
            syntax_doc_url "privatetypes.html#ss:private-types-abbrev";
        }
  | (_, Expression _)
    :: (_, Expression _)
    :: (_, Value_binding _)
    :: (_, Structure_item ({ str_desc = Tstr_value (Recursive, _); _ }, _))
    :: _ ->
      Some
        {
          name = "Recursive value definition";
          description =
            "Supports a certain class of recursive definitions of \
             non-functional values.";
          documentation = syntax_doc_url "letrecvalues.html";
        }
  | (_, Module_expr _) :: (_, Module_type { mty_desc = Tmty_typeof _; _ }) :: _
    ->
      Some
        {
          name = "Recovering module type";
          description =
            "Expands to the module type (signature or functor type) inferred \
             for the module expression `module-expr`. ";
          documentation = syntax_doc_url "moduletypeof.html";
        }
  | (_, Module_expr _)
    :: (_, Module_expr _)
    :: (_, Module_binding _)
    :: (_, Structure_item ({ str_desc = Tstr_recmodule _; _ }, _))
    :: _ ->
      Some
        {
          name = "Recursive module";
          description =
            "A simultaneous definition of modules that can refer recursively \
             to each others.";
          documentation = syntax_doc_url "recursivemodules.html";
        }
  | (_, Expression _)
    :: (_, Expression _)
    :: (_, Case _)
    :: (_, Expression _)
    :: ( _,
         Value_binding
           { vb_expr = { exp_extra = [ (Texp_newtype' _, _, _) ]; _ }; _ } )
    :: _ ->
      Some
        {
          name = "Locally Abstract Type";
          description =
            "Type constructor which is considered abstract in the scope of the \
             sub-expression and replaced by a fresh type variable.";
          documentation = syntax_doc_url "locallyabstract.html";
        }
  | _ :: _
    :: ( _,
         Value_binding
           {
             vb_expr =
               {
                 exp_desc = Texp_pack _;
                 exp_extra =
                   [ (Texp_constraint { ctyp_desc = Ttyp_package _; _ }, _, _) ];
                 _;
               };
             _;
           } )
    :: _ ->
      Some
        {
          name = "First class module";
          description =
            "Converts a module (structure or functor) to a value of the core \
             language that encapsulates the module.";
          documentation = syntax_doc_url "firstclassmodules.html";
        }
  | _ -> None
