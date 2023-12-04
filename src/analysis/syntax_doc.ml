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
        | (_, Type_kind Ttype_open) :: (_, Type_declaration { typ_private = Private; _ }) :: _ ->
          Some { name = "Private Types";
                 description = "Let's you reveal some, but not all aspects of the implementation of a type to clients of the library.";
                 documentation = "https://v2.ocaml.org/releases/5.1/htmlman/privatetypes.html#start-section";
          }
      | (_, Expression _ ) :: (_, Expression _) :: (_, Value_binding _) :: _ -> 
        Some { name = "Recursive definitions of values";
               description = "`let rec` binding construct which also supports a certain class of recursive definitions of non-functional values.";
               documentation = "https://v2.ocaml.org/releases/5.1/htmlman/letrecvalues.html";
        }
      | (_, Module_expr {mod_desc = Tmod_structure _; _}) :: (_, Module_expr { mod_desc = Tmod_constraint _; _ }) :: (_, Module_binding _) :: _ -> 
        Some { name = "Recursive modules"; 
                description = "`module rec ... and` construct to refer recursively to the module identifiers being defined.";
                documentation = "https://v2.ocaml.org/releases/5.1/htmlman/recursivemodules.html";
        }
      | (_, Module_type _ ) :: (_, Module_type_declaration _ ) :: _ -> 
        Some { name = "Recovering a module type";
                description = "`module type` of `module-expr` expands to the module type (signature or functor type) inferred for the module expression module-expr";
                documentation = "https://v2.ocaml.org/releases/5.1/htmlman/moduletypeof.html#start-section";
        }
      | (_, Module_type _ ) :: (_, Include_description _  ) :: (_, Signature_item _) :: (_, Signature _)
      :: (_, Module_type _) :: (_, Module_type_declaration _) :: _ -> 
        Some { name = "Signature Substitution - Destructive substitutions";
                description = "Behaves essentially like normal signature constraints, but it additionally removes the redefined type or module from the signature.";
                documentation = "https://v2.ocaml.org/releases/5.1/htmlman/signaturesubstitution.html#ss:destructive-substitution";
          }
      | (_, Core_type _ ) :: (_, Core_type _  ) :: (_, Value_description _) :: (_, Signature_item _) 
      :: (_, Signature _) :: (_, Module_type _) :: (_, Module_declaration _) :: _ -> 
        Some { name = "Signature Substitution - Local substitution";
                description = "Non recursive way to introduce local names for types and modules when defining a signature";
                documentation = "";
          }
      | (_, Module_expr _) :: (_, Module_expr _) :: (_, Module_binding _) :: (_, Structure_item _) 
      :: (_, Structure _) :: (_, Module_expr _) :: (_, Module_expr _) ::  (_, Module_expr _) ::  (_, Module_binding _) :: _ -> 
        Some { name = "Signature Substitution - Module type substitution"; 
                description = "Good for refining an abstract module type in a signature into a concrete module type or an equivalent module types";
                documentation = "https://v2.ocaml.org/releases/5.1/htmlman/signaturesubstitution.html#ss:module-type-substitution";
        }
      | (_, Structure {str_items = []; _}) :: _ -> 
        Some { name = "Documentation comments"; 
                description = "Automatically converted during parsing into attributes to allow tools to process them as documentation";
                documentation = "https://v2.ocaml.org/releases/5.1/htmlman/doccomments.html";
        }
      | (_, Expression _) :: (_, Expression _) :: _ -> 
        Some { name = "Locally Abstract Type"; 
                description = "Introduces a type constructor which is considered abstract in the scope of the sub-expression, but then replaced by a fresh type variable.";
                documentation = "https://v2.ocaml.org/releases/5.1/htmlman/locallyabstract.html#start-section";
        }
      | _ -> None
      
    end
  in
  match info with
  | Some info -> `Found (Printf.sprintf "%s: %s \nDoc: %s" info.name info.description info.documentation)
  | None -> `No_documentation
