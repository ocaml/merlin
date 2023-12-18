open Browse_raw

type syntax_info = Query_protocol.syntax_doc_result option

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
            "Behaves essentially like normal signature constraints, but it \
             additionally removes the redefined type or module from the \
             signature.";
          documentation =
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
            "Local substitutions behave like destructive substitutions `(with \
             ... := ...)` but instead of being applied to a whole signature \
             after the fact, they are introduced during the specification of \
             the signature, and will apply to all the items that follow.";
          documentation = "signaturesubstitution.html#ss:local-substitution";
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
            "Module type substitution essentially behaves like type \
             substitutions. They are useful to refine an abstract module type \
             in a signature into a concrete module type,";
          documentation =
            "signaturesubstitution.html#ss:module-type-substitution";
        }
  | (_, Constructor_declaration _)
    :: (_, Type_kind (Ttype_variant _))
    :: (_, Type_declaration { typ_private = Public; _ })
    :: _ ->
      Some
        {
          name = "Variant Type";
          description =
            "Let's you represent data that may take on multiple different \
             forms.";
          documentation = "typedecl.html#ss:typedefs";
        }
  | _
    :: (_, Constructor_declaration _)
    :: (_, Type_kind (Ttype_variant _))
    :: (_, Type_declaration { typ_private = Public; _ })
    :: _ ->
      Some
        {
          name = "Variant Type";
          description =
            "Let's you represent data that may take on multiple different \
             forms.";
          documentation = "typedecl.html#ss:typedefs";
        }
  | (_, Type_kind Ttype_open)
    :: (_, Type_declaration { typ_private = Public; _ })
    :: _ ->
      Some
        {
          name = "Extensible variant type";
          description =
            "Can be extended with new variant constructors using +=.";
          documentation = "extensiblevariants.html";
        }
  | (_, Type_kind Ttype_abstract)
    :: (_, Type_declaration { typ_private = Public; _ })
    :: _ ->
      Some
        {
          name = "Abstract type";
          description =
            "Allows you to define variants with arbitrary data structures, \
             including other variants, records, and functions";
          documentation = "typedecl.html#ss:typedefs";
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
          description =
            "Allows you to define variants with a fixed set of fields, and all \
             of the constructors for a record variant type must have the same \
             fields";
          documentation = "typedecl.html#ss:typedefs";
        }
  | (_, Type_kind _) :: (_, Type_declaration { typ_private = Public; _ }) :: _
    ->
      Some
        {
          name = "Empty Variant type";
          description =
            "This extension allows the user to define empty variants.";
          documentation = "emptyvariants.html";
        }
  | (_, Constructor_declaration _)
    :: (_, Type_kind (Ttype_variant _))
    :: (_, Type_declaration { typ_private = Private; _ })
    :: _ ->
      Some
        {
          name = "Private Type";
          description =
            "Values of a variant type declared private can be de-structured \
             normally in pattern-matching. However, values of these types \
             cannot be constructed directly by constructor application.";
          documentation = "privatetypes.html#ss:private-types-variant";
        }
  | _
    :: (_, Constructor_declaration _)
    :: (_, Type_kind (Ttype_variant _))
    :: (_, Type_declaration { typ_private = Private; _ })
    :: _ ->
      Some
        {
          name = "Private Type";
          description =
            "Values of a variant type declared private can be de-structured \
             normally in pattern-matching. However, values of these types \
             cannot be constructed directly by constructor application.";
          documentation = "privatetypes.html#ss:private-types-variant";
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
            "Values of a record type declared private can be de-structured via \
             the expr . field notation. However, values of these types cannot \
             be constructed directly by record construction.";
          documentation = "privatetypes.html#ss:private-types-variant";
        }
  | (_, Type_kind Ttype_open)
    :: (_, Type_declaration { typ_private = Private; _ })
    :: _ ->
      Some
        {
          name = "Private Extensible Variant Type";
          description =
            "Extensible variant types can be declared private. This prevents \
             new constructors from being declared directly, but allows \
             extension constructors to be referred to in interfaces.";
          documentation = "extensiblevariants.html#ss:private-extensible";
        }
  | (_, Type_kind Ttype_abstract)
    :: (_, Type_declaration { typ_private = Private; _ })
    :: _ ->
      Some
        {
          name = "Private Type Abbreviation";
          description =
            "A private type abbreviation declares a type that is distinct from \
             its implementation type `typexpr`.";
          documentation = "privatetypes.html#ss:private-types-abbrev";
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
            "The `let rec` binding construct, in addition to the definition of \
             recursive functions, also supports a certain class of recursive \
             definitions of non-functional values, such as `let rec name1 = 1 \
             :: name2 and name2 = 2 :: name1 in expr` which binds `name1` to \
             the cyclic list `1::2::1::2::…`, and `name2` to the cyclic list \
             `2::1::2::1::…`";
          documentation = "letrecvalues.html";
        }
  | (_, Module_expr _) :: (_, Module_type { mty_desc = Tmty_typeof _; _ }) :: _
    ->
      Some
        {
          name = "Recovering module type";
          description =
            "The construction `module type of module-expr` expands to the \
             module type (signature or functor type) inferred for the module \
             expression `module-expr`. ";
          documentation = "moduletypeof.html";
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
            "Recursive module definitions, introduced by the `module rec …and \
             …` construction, generalize regular module definitions `module \
             module-name = module-expr` and module specifications `module \
             module-name : module-type` by allowing the defining `module-expr` \
             and the `module-type` to refer recursively to the module \
             identifiers being defined.";
          documentation = "recursivemodules.html";
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
            "The expression `fun ( type typeconstr-name ) -> expr` introduces \
             a type constructor named `typeconstr-name` which is considered \
             abstract in the scope of the sub-expression, but then replaced by \
             a fresh type variable.";
          documentation = "locallyabstract.html";
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
            "The expression ( `module module-expr : package-type` ) converts \
             the module (structure or functor) denoted by module expression \
             `module-expr` to a value of the core language that encapsulates \
             this module.";
          documentation = "firstclassmodules.html";
        }
  | _ -> None
