open Browse_raw

type info = {
  name: string;
  description: string;
  documentation: string;
}

let get_syntax_doc node =
  let info =
    begin
      match node with
      | (_, Type_kind _) :: (_, Type_declaration _) :: (_, With_constraint Twith_typesubst _) :: _ -> 
        Some { name = "Signature Substitution - Destructive substitutions";
                description = "Behaves essentially like normal signature constraints, but it additionally removes the redefined type or module \
                               from the signature.";
                documentation = "https://v2.ocaml.org/releases/5.1/htmlman/signaturesubstitution.html#ss:destructive-substitution";
          }
      | (_, Type_kind _) :: (_, Type_declaration _) :: (_, Signature_item ({sig_desc = Tsig_typesubst _; _}, _)) :: _ -> 
        Some { name = "Signature Substitution - Local substitution";
                description = "Local substitutions behave like destructive substitutions `(with ... := ...)` but instead of being applied to a \
                              whole signature after the fact, they are introduced during the specification of the signature, and will apply to \
                              all the items that follow.";
                documentation = "https://v2.ocaml.org/releases/5.1/htmlman/signaturesubstitution.html#ss:local-substitution";
          }
      | (_, Module_type _) :: (_, Module_type _) :: (_, Module_type_constraint Tmodtype_explicit {mty_desc = Tmty_with (_, [_, _, Twith_modtype _]); _} ) :: _ -> 
        Some { name = "Signature Substitution - Module substitution";
                description = "Module type substitution essentially behaves like type substitutions. They are useful to refine an abstract module \
                              type in a signature into a concrete module type,";
                documentation = "https://v2.ocaml.org/releases/5.1/htmlman/signaturesubstitution.html#ss:module-type-substitution";
          }
      | (_, Constructor_declaration _) :: (_, Type_kind Ttype_variant _) :: 
      (_, Type_declaration { typ_private = Public; _ }) :: _ ->
        Some { name = "Variant Types";
               description = "Lets you represent data that may take on multiple different forms.";
               documentation = "https://v2.ocaml.org/manual/coreexamples.html#s:tut-recvariants";
        }
      | (_, Type_kind Ttype_open) :: (_, Type_declaration { typ_private = Public; _ }) :: _ ->
        Some { name = "Extensible variant types";
               description = "Can be extended with new variant constructors using +=.";
               documentation = "https://v2.ocaml.org/manual/extensiblevariants.html";
        }
      | (_, Type_kind Ttype_abstract) :: (_, Type_declaration { typ_private = Public; _ }) :: _ ->
        Some { name = "Abstract variant types";
               description = "Allows you to define variants with arbitrary data structures, including other variants, records, and functions";
               documentation = "https://v2.ocaml.org/manual/extensiblevariants.html";
        }
      | (_, Type_kind Ttype_record _) :: (_, Type_declaration { typ_private = Public; _ }) :: _ ->
        Some { name = "Record variant types";
               description = "Allows you to define variants with a fixed set of fields, and all of the constructors for a record variant type must have the same fields";
               documentation = "https://v2.ocaml.org/manual/extensiblevariants.html";
        }
      | (_, Type_kind _) :: (_, Type_declaration { typ_private = Public; _ }) :: _ ->
        Some { name = "Empty variant types";
               description = "This extension allows the user to define empty variants.";
               documentation = "https://v2.ocaml.org/manual/emptyvariants.html";
        }
      | _ :: (_, Constructor_declaration _) :: (_, Type_kind Ttype_variant _ ) :: (_, Type_declaration { typ_private = Private; _ }) :: _ ->
        Some { name = "Private Variant Types";
                description = "Values of a variant type declared private can be de-structured normally \
                              in pattern-matching. However, values of these types cannot be constructed directly by constructor application.";
                documentation = "https://v2.ocaml.org/releases/5.1/htmlman/privatetypes.html#ss:private-types-variant";
        }
      |_ :: _ :: (_, Label_declaration _) :: (_, Type_kind Ttype_record _ ) :: (_, Type_declaration { typ_private = Private; _ }) :: _ ->
        Some { name = "Private Record Types";
                description = "Values of a record type declared private can be de-structured via the expr . field notation. \
                              However, values of these types cannot be constructed directly by record construction.";
                documentation = "https://v2.ocaml.org/releases/5.1/htmlman/privatetypes.html#ss:private-types-variant";
        }
      | (_, Type_kind Ttype_open) :: (_, Type_declaration { typ_private = Private; _ }) :: _ ->
        Some { name = "Private Extensible Types";
                description = "Enable libraries to reveal some, but not all aspects of the implementation of a type to \
                              clients of the library";
                documentation = "https://v2.ocaml.org/releases/5.1/htmlman/privatetypes.html#start-section";
        }
      | (_, Type_kind Ttype_abstract) :: (_, Type_declaration { typ_private = Private; _ }) :: _ ->
        Some { name = "Private Type Abbreviations";
                description = "A private type abbreviation declares a type that is distinct from its implementation type `typexpr`.";
                documentation = "https://v2.ocaml.org/releases/5.1/htmlman/privatetypes.html#ss:private-types-abbrev";
        }
      | (_, Expression _ ) :: (_, Expression _) :: (_, Value_binding _) :: (_, Structure_item ({str_desc = Tstr_value (Recursive, _); _}, _)) :: _ -> 
        Some { name = "Recursive definitions of values";
               description = "The `let rec` binding construct, in addition to the definition of recursive functions, also supports a certain \
                              class of recursive definitions of non-functional values, such as \
                              `let rec name1 = 1 :: name2 and name2 = 2 :: name1 in expr` which binds `name1` to the cyclic list `1::2::1::2::…`, \
                              and `name2` to the cyclic list `2::1::2::1::…`";
               documentation = "https://v2.ocaml.org/releases/5.1/htmlman/letrecvalues.html";
        }
      | [_, Structure _] -> 
        Some { name = "Documentation comments"; 
                description = "Automatically converted during parsing into attributes to allow tools to process them as documentation";
                documentation = "https://v2.ocaml.org/releases/5.1/htmlman/doccomments.html";
        } 
      | (_, Module_expr _) :: (_, Module_type {mty_desc = Tmty_typeof _; _}) :: _ ->
        Some { name = "Recovering the type of a module"; 
                description = "The construction `module type of module-expr` expands to the module type (signature or functor type) inferred \
                               for the module expression `module-expr`. ";
                documentation = "https://v2.ocaml.org/releases/5.1/htmlman/moduletypeof.html";
        }
      | node -> 
        let n = List.rev node in 
        match n with 
        | _ :: (_, Structure_item ({str_desc = Tstr_recmodule _; _}, _)) :: _  -> 
          Some { name = "Recursive modules"; 
                  description = "Recursive module definitions, introduced by the `module rec …and …` construction, \
                                generalize regular module definitions `module module-name = module-expr` and module \
                                specifications `module module-name : module-type` by allowing the defining `module-expr` \
                                and the `module-type` to refer recursively to the module identifiers being defined.";
                  documentation = "https://v2.ocaml.org/releases/5.1/htmlman/recursivemodules.html";
          }
        | _ :: _ :: (_, Value_binding {vb_expr = {exp_extra = ([Texp_newtype' _,_,_]); _}; _}) :: _ ->
          Some { name = "Locally Abstract Type"; 
                  description = "The expression `fun ( type typeconstr-name ) -> expr` introduces a type constructor named `typeconstr-name` which \
                                  is considered abstract in the scope of the sub-expression, but then replaced by a fresh type variable.";
                  documentation = "https://v2.ocaml.org/releases/5.1/htmlman/locallyabstract.html";
          }
        | _ :: _ :: (_, Value_binding {vb_expr = {exp_desc = (Texp_pack _); exp_extra = [(Texp_constraint {ctyp_desc = Ttyp_package _; _}, _, _)]; _}; _}) :: _ ->
          Some { name = "First class modules"; 
                  description = "The expression ( `module module-expr : package-type` ) converts the module (structure or functor) denoted by module \
                                 expression `module-expr` to a value of the core language that encapsulates this module.";
                  documentation = "https://v2.ocaml.org/releases/5.1/htmlman/firstclassmodules.html";
          }
      | _ -> None      
    end
  in
  match info with
  | Some info -> `Found (Printf.sprintf "%s: \n%s \n%s" info.name info.description info.documentation)
  | None -> `No_documentation
