open Ocamlbuild_plugin ;;

dispatch begin function
  | After_rules ->
    flag ["ocaml" ; "menhir"; "use_new_menhir"]
      (S [A "--table"; A"--typed-values"; A"--stepwise"]);
  | _ -> ()
end

