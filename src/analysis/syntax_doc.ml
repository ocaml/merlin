open Browse_raw

type info = {
  name: string;
  description: string;
  documentation: string;
}

let get_syntax_doc node_parent node =
  let info = 
    begin 
      match node_parent, node with
      | (_, Type_kind _) :: _, (_, Constructor_declaration _) :: _ ->  
       Some { name = "Variant Types"; 
          description = "Lets you represent data that may take on multiple different forms."; 
          documentation = "https://v2.ocaml.org/manual/coreexamples.html#s:tut-recvariants"; 
          }
      | (_, Type_declaration _) :: _, (_, Type_kind Ttype_open) :: _ ->  
       Some { name = "Extensible variant types"; 
          description = "Can be extended with new variant constructors using +=.";
          documentation = "https://v2.ocaml.org/manual/extensiblevariants.html"; 
          }
      | (_, Type_declaration _) :: _, (_, Type_kind Ttype_abstract) :: _ ->  
        Some { name = "Abstract variant types"; 
            description = "An abstract variant type";
            documentation = "https://v2.ocaml.org/manual/"; 
          }
        | (_, Type_declaration _) :: _, (_, Type_kind Ttype_record _) :: _ ->  
          Some { name = "Record variant types"; 
              description = "A record variant type";
              documentation = "https://v2.ocaml.org/manual/"; 
            }
      | (_, Type_declaration _) :: _, (_, Type_kind _) :: _ ->  
        Some { name = "Empty variant types"; 
            description = "This extension allows user to define empty variants.";
            documentation = "https://v2.ocaml.org/manual/emptyvariants.html"; 
          }
      | _ -> None
    end
    in 
    (match info with 
    | Some info -> `Found (Printf.sprintf "%s: %s \n%s" info.name info.description info.documentation)
    | None -> `No_documentation);