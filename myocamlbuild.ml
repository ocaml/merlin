open Ocamlbuild_plugin ;;

dispatch begin function
  | After_rules ->
     flag [ "ocaml" ; "menhir";"use_menhir_table" ] (S[A "--table"]);
  | _ -> ()
end

