open Ocamlbuild_plugin ;;

dispatch begin function
  | After_rules ->
      (* Menhir --table *)
      flag ["ocaml" ; "menhir"; "use_menhir_table"] (S [A "--table"]);
      
      (* Menhir --external-tokens and --base flag *)
      flag ["ocaml"; "parser"; "menhir"; "ext_tokens"]
        (S [ A"--external-tokens";
             A"Chunk_parser"]);
             (*A"chunk_parser.mly"]);*)
      flag ["ocaml"; "menhir_ocamldep"; "ext_tokens"]
        (S [ A"--external-tokens";
             A"Chunk_parser"])
             (*A"chunk_parser.mly"]);*)
        
  | _ -> ()
end

