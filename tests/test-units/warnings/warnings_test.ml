open Merlin_analysis

let test_description () =
  match Misc_utils.warning_description 1 with
  | Some d ->
    Alcotest.(check int) "number" 1 d.Ocaml_utils.Warnings.number;
    Alcotest.(check (list string)) "names" [ "comment-start" ] d.Ocaml_utils.Warnings.names;
    Alcotest.(check string)
      "description" "Suspicious-looking start-of-comment mark." d.Ocaml_utils.Warnings.description
  | None -> Alcotest.fail "Expected warning 1 to have a description"

let test_description_not_found () =
  match Misc_utils.warning_description 999 with
  | Some _ -> Alcotest.fail "Expected warning 999 to not have a description"
  | None -> ()

let () =
  let open Alcotest in
  run "Warnings"
    [ ( "description",
        [ test_case "found" `Quick test_description;
          test_case "not found" `Quick test_description_not_found
        ] )
    ]
