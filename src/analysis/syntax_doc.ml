open Browse_raw

type syntax_info = Query_protocol.syntax_doc_result

let get_syntax_doc node : syntax_info =
  match node with
  | (_, Type_kind _)
    :: (_, Type_declaration _)
    :: (_, With_constraint (Twith_typesubst _))
    :: _ ->
      {
        name = "Signature Substitution - Destructive substitutions";
        description =
          "Behaves essentially like normal signature constraints, but it \
           additionally removes the redefined type or module from the \
           signature.";
        documentation =
          "https://v2.ocaml.org/releases/5.1/htmlman/signaturesubstitution.html#ss:destructive-substitution";
      }
  | (_, Type_kind _)
    :: (_, Type_declaration _)
    :: (_, Signature_item ({ sig_desc = Tsig_typesubst _; _ }, _))
    :: _ ->
      {
        name = "Signature Substitution - Local substitution";
        description =
          "Local substitutions behave like destructive substitutions `(with \
           ... := ...)` but instead of being applied to a whole signature \
           after the fact, they are introduced during the specification of the \
           signature, and will apply to all the items that follow.";
        documentation =
          "https://v2.ocaml.org/releases/5.1/htmlman/signaturesubstitution.html#ss:local-substitution";
      }
  | (_, Module_type _)
    :: (_, Module_type _)
    :: ( _,
         Module_type_constraint
           (Tmodtype_explicit
             { mty_desc = Tmty_with (_, [ (_, _, Twith_modtype _) ]); _ }) )
    :: _ ->
      {
        name = "Signature Substitution - Module substitution";
        description =
          "Module type substitution essentially behaves like type \
           substitutions. They are useful to refine an abstract module type in \
           a signature into a concrete module type,";
        documentation =
          "https://v2.ocaml.org/releases/5.1/htmlman/signaturesubstitution.html#ss:module-type-substitution";
      }
  | (_, Constructor_declaration _)
    :: (_, Type_kind (Ttype_variant _))
    :: (_, Type_declaration { typ_private = Public; _ })
    :: _ ->
      {
        name = "Variant Types";
        description =
          "Let's you represent data that may take on multiple different forms.";
        documentation =
          "https://v2.ocaml.org/manual/coreexamples.html#s:tut-recvariants";
      }
  | _
    :: (_, Constructor_declaration _)
    :: (_, Type_kind (Ttype_variant _))
    :: (_, Type_declaration { typ_private = Public; _ })
    :: _ ->
      {
        name = "Variant Types";
        description =
          "Let's you represent data that may take on multiple different forms.";
        documentation =
          "https://v2.ocaml.org/manual/coreexamples.html#s:tut-recvariants";
      }
  | (_, Type_kind Ttype_open)
    :: (_, Type_declaration { typ_private = Public; _ })
    :: _ ->
      {
        name = "Extensible variant types";
        description = "Can be extended with new variant constructors using +=.";
        documentation = "https://v2.ocaml.org/manual/extensiblevariants.html";
      }
  | (_, Type_kind Ttype_abstract)
    :: (_, Type_declaration { typ_private = Public; _ })
    :: _ ->
      {
        name = "Abstract types";
        description =
          "Allows you to define variants with arbitrary data structures, \
           including other variants, records, and functions";
        documentation = "https://v2.ocaml.org/manual/extensiblevariants.html";
      }
  | (_, Core_type _)
    :: (_, Core_type _)
    :: (_, Label_declaration _)
    :: (_, Type_kind (Ttype_record _))
    :: (_, Type_declaration { typ_private = Public; _ })
    :: _ ->
      {
        name = "Record types";
        description =
          "Allows you to define variants with a fixed set of fields, and all \
           of the constructors for a record variant type must have the same \
           fields";
        documentation = "https://v2.ocaml.org/manual/extensiblevariants.html";
      }
  | (_, Type_kind _) :: (_, Type_declaration { typ_private = Public; _ }) :: _
    ->
      {
        name = "Empty Variant types";
        description = "This extension allows the user to define empty variants.";
        documentation = "https://v2.ocaml.org/manual/emptyvariants.html";
      }
  | (_, Constructor_declaration _)
    :: (_, Type_kind (Ttype_variant _))
    :: (_, Type_declaration { typ_private = Private; _ })
    :: _ ->
      {
        name = "Private Types";
        description =
          "Values of a variant type declared private can be de-structured \
           normally in pattern-matching. However, values of these types cannot \
           be constructed directly by constructor application.";
        documentation =
          "https://v2.ocaml.org/releases/5.1/htmlman/privatetypes.html#ss:private-types-variant";
      }
  | _
    :: (_, Constructor_declaration _)
    :: (_, Type_kind (Ttype_variant _))
    :: (_, Type_declaration { typ_private = Private; _ })
    :: _ ->
      {
        name = "Private Types";
        description =
          "Values of a variant type declared private can be de-structured \
           normally in pattern-matching. However, values of these types cannot \
           be constructed directly by constructor application.";
        documentation =
          "https://v2.ocaml.org/releases/5.1/htmlman/privatetypes.html#ss:private-types-variant";
      }
  | _ :: _
    :: (_, Label_declaration _)
    :: (_, Type_kind (Ttype_record _))
    :: (_, Type_declaration { typ_private = Private; _ })
    :: _ ->
      {
        name = "Private Record Types";
        description =
          "Values of a record type declared private can be de-structured via \
           the expr . field notation. However, values of these types cannot be \
           constructed directly by record construction.";
        documentation =
          "https://v2.ocaml.org/releases/5.1/htmlman/privatetypes.html#ss:private-types-variant";
      }
  | (_, Type_kind Ttype_open)
    :: (_, Type_declaration { typ_private = Private; _ })
    :: _ ->
      {
        name = "Private Extensible Types";
        description =
          "Enable libraries to reveal , but not all aspects of the \
           implementation of a type to clients of the library";
        documentation =
          "https://v2.ocaml.org/releases/5.1/htmlman/privatetypes.html#start-section";
      }
  | (_, Type_kind Ttype_abstract)
    :: (_, Type_declaration { typ_private = Private; _ })
    :: _ ->
      {
        name = "Private Type Abbreviations";
        description =
          "A private type abbreviation declares a type that is distinct from \
           its implementation type `typexpr`.";
        documentation =
          "https://v2.ocaml.org/releases/5.1/htmlman/privatetypes.html#ss:private-types-abbrev";
      }
  | (_, Expression _)
    :: (_, Expression _)
    :: (_, Value_binding _)
    :: (_, Structure_item ({ str_desc = Tstr_value (Recursive, _); _ }, _))
    :: _ ->
      {
        name = "Recursive definitions of values";
        description =
          "The `let rec` binding construct, in addition to the definition of \
           recursive functions, also supports a certain class of recursive \
           definitions of non-functional values, such as `let rec name1 = 1 :: \
           name2 and name2 = 2 :: name1 in expr` which binds `name1` to the \
           cyclic list `1::2::1::2::…`, and `name2` to the cyclic list \
           `2::1::2::1::…`";
        documentation =
          "https://v2.ocaml.org/releases/5.1/htmlman/recvalues.html";
      }
  | [ (_, Structure _) ] ->
      {
        name = "Documentation comments";
        description =
          "Automatically converted during parsing into attributes to allow \
           tools to process them as documentation";
        documentation =
          "https://v2.ocaml.org/releases/5.1/htmlman/doccomments.html";
      }
  | (_, Module_expr _) :: (_, Module_type { mty_desc = Tmty_typeof _; _ }) :: _
    ->
      {
        name = "Recovering the type of a module";
        description =
          "The construction `module type of module-expr` expands to the module \
           type (signature or functor type) inferred for the module expression \
           `module-expr`. ";
        documentation =
          "https://v2.ocaml.org/releases/5.1/htmlman/moduypeof.html";
      }
  | (_, Module_expr _)
    :: (_, Module_expr _)
    :: (_, Module_binding _)
    :: (_, Structure_item ({ str_desc = Tstr_recmodule _; _ }, _))
    :: _ ->
      {
        name = "Recursive modules";
        description =
          "Recursive module definitions, introduced by the `module rec …and …` \
           construction, generalize regular module definitions `module \
           module-name = module-expr` and module specifications `module \
           module-name : module-type` by allowing the defining `module-expr` \
           and the `module-type` to refer recursively to the module \
           identifiers being defined.";
        documentation =
          "https://v2.ocaml.org/releases/5.1/htmlman/recursivemodules.html";
      }
  | (_, Expression _)
    :: (_, Expression _)
    :: (_, Case _)
    :: (_, Expression _)
    :: ( _,
         Value_binding
           { vb_expr = { exp_extra = [ (Texp_newtype' _, _, _) ]; _ }; _ } )
    :: _ ->
      {
        name = "Locally Abstract Type";
        description =
          "The expression `fun ( type typeconstr-name ) -> expr` introduces a \
           type constructor named `typeconstr-name` which is considered \
           abstract in the scope of the sub-expression, but then replaced by a \
           fresh type variable.";
        documentation =
          "https://v2.ocaml.org/releases/5.1/htmlman/locallyabstract.html";
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
      {
        name = "First class modules";
        description =
          "The expression ( `module module-expr : package-type` ) converts the \
           module (structure or functor) denoted by module expression \
           `module-expr` to a value of the core language that encapsulates \
           this module.";
        documentation =
          "https://v2.ocaml.org/releases/5.1/htmlman/firstclassmodules.html";
      }
  | (_, Expression _)
    :: (_, Expression _)
    :: (_, Expression _)
    :: (_, Expression _)
    :: (_, Expression _)
    :: (_, Expression _)
    :: _ ->
      {
        name = "Syntax for Bigarray Access";
        description =
          "This extension provides syntactic sugar for getting and setting \
           elements in the arrays provided by the Bigarray module.";
        documentation =
          "https://v2.ocaml.org/releases/5.1/htmlman/bigarray.html";
      }
  | _ -> { name = ""; description = ""; documentation = "" }
