let ocaml_version_val =
  match Scanf.sscanf Sys.argv.(1) "%s@.%s@." (fun maj min -> maj, min) with
  | "4", "02" ->
      "`OCaml_4_02_3"
  | maj, min ->
      Printf.sprintf "`OCaml_%s_%s_0" maj min

let () =
  Printf.printf {|
let version = "%%VERSION%%"
let ocamlversion :
  [ `OCaml_4_02_0 | `OCaml_4_02_1 | `OCaml_4_02_2 | `OCaml_4_02_3
  | `OCaml_4_03_0 | `OCaml_4_04_0 | `OCaml_4_05_0 | `OCaml_4_06_0
  | `OCaml_4_07_0 ] = %s
|} ocaml_version_val
