open Ocamlbuild_plugin ;;

dispatch begin function
  | After_rules ->
    (* Menhir --table *)
    mark_tag_used "use_menhir";
    mark_tag_used "use_menhir_table";
    flag ["ocaml" ; "menhir"; "use_new_menhir"]
      (S [A "--table"; A"--typed-values"; A"--stepwise"]);
    (* Menhir --external-tokens and --base flag *)
    flag ["ocaml"; "parser"; "menhir"; "ext_tokens"]
      (S [A"--external-tokens"; A"Raw_parser"]);
    flag ["ocaml"; "menhir_ocamldep"; "ext_tokens"]
      (S [A"--external-tokens"; A"Raw_parser"]);
  | _ -> ()
end

