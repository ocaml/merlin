open Browse_raw

type syntax_info = Query_protocol.syntax_doc_result option

let syntax_doc_url endpoint =
  let base_url = "https://v2.ocaml.org/releases/4.14/htmlman/" in
  base_url ^ endpoint

let get_syntax_doc cursor_loc node : syntax_info =
  match node with
  | (_, Type_kind _)
    :: (_, Type_declaration _)
    :: (_, With_constraint (Twith_typesubst _))
    :: _ ->
    Some
      { name = "Destructive substitution";
        description =
          "Behaves like normal signature constraints but removes the redefined \
           type or module from the signature.";
        documentation =
          syntax_doc_url
            "signaturesubstitution.html#ss:destructive-substitution"
      }
  | (_, Type_kind _)
    :: (_, Type_declaration _)
    :: (_, Signature_item ({ sig_desc = Tsig_typesubst _; _ }, _))
    :: _ ->
    Some
      { name = "Local substitution";
        description =
          "Behaves like destructive substitution but is introduced during the \
           specification of the signature, and will apply to all the items \
           that follow.";
        documentation =
          syntax_doc_url "signaturesubstitution.html#ss:local-substitution"
      }
  | (_, Module_type _)
    :: (_, Module_type _)
    :: ( _,
         Module_type_constraint
           (Tmodtype_explicit
             { mty_desc = Tmty_with (_, [ (_, _, Twith_modtype _) ]); _ }) )
    :: _ ->
    Some
      { name = "Module substitution";
        description =
          "Behaves like type substitutions but are useful to refine an \
           abstract module type in a signature into a concrete module type,";
        documentation =
          syntax_doc_url
            "signaturesubstitution.html#ss:module-type-substitution"
      }
  | (_, Type_kind Ttype_open) :: (_, Type_declaration { typ_private; _ }) :: _
    ->
    let e_name = "Extensible Variant Type" in
    let e_description =
      "Can be extended with new variant constructors using `+=`."
    in
    let e_url = "extensiblevariants.html" in
    let name, description, url =
      match typ_private with
      | Public -> (e_name, e_description, e_url)
      | Private ->
        ( Format.sprintf "Private %s" e_name,
          Format.sprintf
            "%s. Prevents new constructors from being declared directly, but \
             allows extension constructors to be referred to in interfaces."
            e_description,
          "extensiblevariants.html#ss:private-extensible" )
    in
    Some { name; description; documentation = syntax_doc_url url }
  | (_, Constructor_declaration _)
    :: (_, Type_kind (Ttype_variant _))
    :: (_, Type_declaration { typ_private; _ })
    :: _
  | _
    :: (_, Constructor_declaration _)
    :: (_, Type_kind (Ttype_variant _))
    :: (_, Type_declaration { typ_private; _ })
    :: _ ->
    let v_name = "Variant Type" in
    let v_description =
      "Represent's data that may take on multiple different forms."
    in
    let v_url = "typedecl.html#ss:typedefs" in
    let name, description, url =
      match typ_private with
      | Public -> (v_name, v_description, v_url)
      | Private ->
        ( Format.sprintf "Private %s" v_name,
          Format.sprintf
            "%s This type is private, values cannot be constructed directly \
             but can be de-structured as usual."
            v_description,
          "privatetypes.html#ss:private-types-variant" )
    in
    Some { name; description; documentation = syntax_doc_url url }
  | (_, Core_type _)
    :: (_, Core_type _)
    :: (_, Label_declaration _)
    :: (_, Type_kind (Ttype_record _))
    :: (_, Type_declaration { typ_private; _ })
    :: _ ->
    let r_name = "Record Type" in
    let r_description = "Defines variants with a fixed set of fields" in
    let r_url = "typedecl.html#ss:typedefs" in
    let name, description, url =
      match typ_private with
      | Public -> (r_name, r_description, r_url)
      | Private ->
        ( Format.sprintf "Private %s" r_name,
          Format.sprintf
            "%s This type is private, values cannot be constructed directly \
             but can be de-structured as usual."
            r_description,
          "privatetypes.html#ss:private-types-variant" )
    in
    Some { name; description; documentation = syntax_doc_url url }
  | (_, Type_kind (Ttype_variant _))
    :: (_, Type_declaration { typ_private = Public; _ })
    :: _ ->
    Some
      { name = "Empty Variant Type";
        description = "An empty variant type.";
        documentation = syntax_doc_url "emptyvariants.html"
      }
  | (_, Type_kind Ttype_abstract)
    :: (_, Type_declaration { typ_private = Public; typ_manifest = None; _ })
    :: _ ->
    Some
      { name = "Abstract Type";
        description =
          "Define variants with arbitrary data structures, including other \
           variants, records, and functions";
        documentation = syntax_doc_url "typedecl.html#ss:typedefs"
      }
  | (_, Type_kind Ttype_abstract)
    :: (_, Type_declaration { typ_private = Private; _ })
    :: _ ->
    Some
      { name = "Private Type Abbreviation";
        description =
          "Declares a type that is distinct from its implementation type \
           `typexpr`.";
        documentation =
          syntax_doc_url "privatetypes.html#ss:private-types-abbrev"
      }
  | (_, Expression _)
    :: (_, Expression _)
    :: (_, Value_binding _)
    :: (_, Structure_item ({ str_desc = Tstr_value (Recursive, _); _ }, _))
    :: _ ->
    Some
      { name = "Recursive value definition";
        description =
          "Supports a certain class of recursive definitions of non-functional \
           values.";
        documentation = syntax_doc_url "letrecvalues.html"
      }
  | (_, Module_expr _) :: (_, Module_type { mty_desc = Tmty_typeof _; _ }) :: _
    ->
    Some
      { name = "Recovering module type";
        description =
          "Expands to the module type (signature or functor type) inferred for \
           the module expression `module-expr`. ";
        documentation = syntax_doc_url "moduletypeof.html"
      }
  | (_, Module_expr _)
    :: (_, Module_expr _)
    :: (_, Module_binding _)
    :: (_, Structure_item ({ str_desc = Tstr_recmodule _; _ }, _))
    :: _ ->
    Some
      { name = "Recursive module";
        description =
          "A simultaneous definition of modules that can refer recursively to \
           each others.";
        documentation = syntax_doc_url "recursivemodules.html"
      }
  | (_, Expression _)
    :: (_, Expression _)
    :: (_, Expression _)
    :: ( _,
         Value_binding
           { vb_expr =
               { exp_extra = [ (Texp_newtype' (_, loc, _), _, _) ]; exp_loc; _ };
             _
           } )
    :: _ -> (
    let in_range =
      cursor_loc.Lexing.pos_cnum - 1 > exp_loc.loc_start.pos_cnum
      && cursor_loc.Lexing.pos_cnum <= loc.loc.loc_end.pos_cnum + 1
    in
    match in_range with
    | true ->
      Some
        { name = "Locally Abstract Type";
          description =
            "Type constructor which is considered abstract in the scope of the \
             sub-expression and replaced by a fresh type variable.";
          documentation = syntax_doc_url "locallyabstract.html"
        }
    | false -> None)
  | (_, Module_expr _)
    :: (_, Module_expr _)
    :: (_, Expression { exp_desc = Texp_pack _; _ })
    :: _ ->
    Some
      { name = "First class module";
        description =
          "Converts a module (structure or functor) to a value of the core \
           language that encapsulates the module.";
        documentation = syntax_doc_url "firstclassmodules.html"
      }
  | _ -> None
