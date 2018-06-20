let ocaml_version_val s =
  let maj, min, patch =
    Scanf.sscanf s "%[0-9].%[0-9].%[0-9]" (fun maj min patch -> maj, min, patch)
  in
  match maj, min, patch with
  | "4", "02", "0" ->
     "`OCaml_4_02_0"
  | "4", "02", "1" ->
     "`OCaml_4_02_1"
  | "4", "02", "2" ->
     "`OCaml_4_02_2"
  | "4", "02", "3" ->
     "`OCaml_4_02_3"
  | "4", "04", _ ->
     "`OCaml_4_04_0"
  | "4", "05", _ ->
     "`OCaml_4_05_0"
  | "4", "06", _ ->
     "`OCaml_4_06_0"
  | "4", "07", _ ->
     "`OCaml_4_07_0"
  | _ ->
     assert false
;;

Printf.ksprintf print_endline
  {|let version = "unknown"
let ocamlversion : [ `OCaml_4_02_0 | `OCaml_4_02_1 | `OCaml_4_02_2 | `OCaml_4_02_3 | `OCaml_4_03_0 | `OCaml_4_04_0 | `OCaml_4_05_0 | `OCaml_4_06_0 | `OCaml_4_07_0 | `OCaml_unknown ] = %s|} (ocaml_version_val Sys.argv.(1))
;;
