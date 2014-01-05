open Ocamlbuild_plugin ;;

dispatch begin function
  | After_rules ->
    (* Menhir --table *)
    flag ["ocaml" ; "menhir"; "use_new_menhir"]
      (S [A "--table"; A"--typed-values"; A"--stepwise"]);
    (* Menhir --external-tokens and --base flag *)
    flag ["ocaml"; "parser"; "menhir"; "ext_tokens"]
      (S [A"--external-tokens"; A"Raw_parser"]);
    flag ["ocaml"; "menhir_ocamldep"; "ext_tokens"]
      (S [A"--external-tokens"; A"Raw_parser"]);
    (* Lexer *)
    flag ["ocaml"; "ocamllex"; "use_lexer_refill_handler"]
      (S [A"-refill-handler"; A"refill_handler"])
  | _ -> ()
end

