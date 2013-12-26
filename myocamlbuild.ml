open Ocamlbuild_plugin ;;

dispatch begin function
  | After_rules ->
    (* Menhir --table *)
    flag ["ocaml" ; "menhir"; "use_new_menhir"]
      (S [A "--table"; A"--typed-values"; A"--stepwise"]);
    (* Menhir --external-tokens and --base flag *)
    flag ["ocaml"; "parser"; "menhir"; "ext_tokens"]
      (S [A"--external-tokens"; A"Chunk_parser"]);
    flag ["ocaml"; "menhir_ocamldep"; "ext_tokens"]
      (S [A"--external-tokens"; A"Chunk_parser"])
  | _ -> ()
end

