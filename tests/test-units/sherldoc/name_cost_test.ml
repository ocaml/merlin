open Merlin_sherlodoc

let test_distance_1 =
  let open Alcotest in
  test_case "test distance - 1" `Quick (fun () ->
      let expected = List.map Option.some [ 0; 1; 1; 1; 1; 2; 2; 2; 2 ]
      and computed =
        List.map
          (Name_cost.distance "decode")
          [ "decode";
            "decade";
            "decede";
            "decide";
            "recode";
            "bbcode";
            "become";
            "code";
            "derobe"
          ]
      in
      check (list @@ option int) "should be equal" expected computed)

let test_distance_2 =
  let open Alcotest in
  test_case "test distance - 2" `Quick (fun () ->
      let expected = Some 1
      and computed = Name_cost.distance "Foo.Bar.Baz" "Foo_Bar.Baz" in
      check (option int) "should be equal" expected computed)

let test_distance_3 =
  let open Alcotest in
  test_case "test distance - 3" `Quick (fun () ->
      let expected = Some 2
      and computed = Name_cost.distance "Ltw_mutex" "Lwt_mutex" in
      check (option int) "should be equal" expected computed)

let test_distance_4 =
  let open Alcotest in
  test_case "test distance - 4" `Quick (fun () ->
      let expected = Some 4
      and computed = Name_cost.distance "Foo_Bar_Baz" "Bar_Baz" in
      check (option int) "should be equal" expected computed)

let test_distance_5 =
  let open Alcotest in
  test_case "test distance - 5" `Quick (fun () ->
      let expected = None
      and computed =
        Name_cost.distance ~cutoff:16 "Ocaml_typing.Misc.f" "Bar_Baz"
      in
      check (option int) "should be equal" expected computed)

let test_distance_substring_1 =
  let open Alcotest in
  test_case "test distance_substring - 1" `Quick (fun () ->
      let expected = Some 2
      and computed = Name_cost.distance_of_substring "Foo" "Bar.Foo.Baz" in
      check (option int) "should be equal" expected computed)

let test_distance_substring_2 =
  let open Alcotest in
  test_case "test distance_substring - 2" `Quick (fun () ->
      let expected = Some 5
      and computed = Name_cost.distance_of_substring "Foo" "Bar.oFo.Baz" in
      check (option int) "should be equal" expected computed)

let test_distance_substring_3 =
  let open Alcotest in
  test_case "test distance_substring - 3" `Quick (fun () ->
      let expected = Some 0
      and computed = Name_cost.distance_of_substring "Foo" "Foo" in
      check (option int) "should be equal" expected computed)

let test_distance_substring_4 =
  let open Alcotest in
  test_case "test distance_substring - 4" `Quick (fun () ->
      let expected = Some 4
      and computed = Name_cost.distance_of_substring "Foo" "Hashtblk" in
      check (option int) "should be equal" expected computed)

let test_best_distance_1 =
  let open Alcotest in
  test_case "test bast distance - 1" `Quick (fun () ->
      let expected = 2
      and computed =
        Name_cost.best_distance [ "bz"; "dddd"; "Foo" ] "Bar.Foo.Baz"
      in
      check int "should be equal" expected computed)

let test_best_distance_2 =
  let open Alcotest in
  test_case "test bast distance - 2" `Quick (fun () ->
      let expected = 4
      and computed =
        Name_cost.best_distance [ "bz"; "dddd"; "oFo" ] "Bar.Foo.Baz"
      in
      check int "should be equal" expected computed)

let test_best_distance_3 =
  let open Alcotest in
  test_case "test bast distance - 3" `Quick (fun () ->
      let expected = 5
      and computed =
        Name_cost.best_distance
          [ "bsadsadz"; "dddd"; "moduleHassh" ]
          "Bar.Foo.Baz"
      in
      check int "should be equal" expected computed)

let cases =
  ( "name_cost",
    [ test_distance_1;
      test_distance_2;
      test_distance_3;
      test_distance_4;
      test_distance_5;
      test_distance_substring_1;
      test_distance_substring_2;
      test_distance_substring_3;
      test_distance_substring_4;
      test_best_distance_1;
      test_best_distance_2;
      test_best_distance_3
    ] )
