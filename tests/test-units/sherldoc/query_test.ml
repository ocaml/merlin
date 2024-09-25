open Merlin_sherlodoc

let test_distance_1 =
  let open Alcotest in
  test_case "test distance from a query - 1" `Quick (fun () ->
      let query = "List.map"
      and path = "List.map"
      and candidate = "('a -> 'b) -> 'a list -> 'b list" in
      let expected = 0
      and computed =
        Query.(
          distance_for (from_string query) ~path
            (candidate |> Type_expr.from_string |> Option.get))
      in
      check int "should be equal" expected computed)

let test_distance_2 =
  let open Alcotest in
  test_case "test distance from a query - 2" `Quick (fun () ->
      let query = "List.map : ('f -> 'g) -> 'f list -> 'g list"
      and path = "List.map"
      and candidate = "('a -> 'b) -> 'a list -> 'b list" in
      let expected = 0
      and computed =
        Query.(
          distance_for (from_string query) ~path
            (candidate |> Type_expr.from_string |> Option.get))
      in
      check int "should be equal" expected computed)

let test_distance_3 =
  let open Alcotest in
  test_case "test distance from a query - 3" `Quick (fun () ->
      let query = "('f -> 'g) -> 'f list -> 'g list"
      and path = "List.map"
      and candidate = "('a -> 'b) -> 'a list -> 'b list" in
      let expected = 0
      and computed =
        Query.(
          distance_for (from_string query) ~path
            (candidate |> Type_expr.from_string |> Option.get))
      in
      check int "should be equal" expected computed)

let test_distance_4 =
  let open Alcotest in
  test_case "test distance from a query - 4" `Quick (fun () ->
      let query = "map : ('f -> 'g) -> 'f list -> 'g list"
      and path = "List.map"
      and candidate = "('a -> 'b) -> 'a list -> 'b list" in
      let expected = 1
      and computed =
        Query.(
          distance_for (from_string query) ~path
            (candidate |> Type_expr.from_string |> Option.get))
      in
      check int "should be equal" expected computed)

let test_distance_5 =
  let open Alcotest in
  test_case "test distance from a query - 5" `Quick (fun () ->
      let query = "map : 'f list -> ('f -> 'g) -> 'g list"
      and path = "List.map"
      and candidate = "('a -> 'b) -> 'a list -> 'b list" in
      let expected = 1
      and computed =
        Query.(
          distance_for (from_string query) ~path
            (candidate |> Type_expr.from_string |> Option.get))
      in
      check int "should be equal" expected computed)

let test_distance_6 =
  let open Alcotest in
  test_case "test distance from a query - 6" `Quick (fun () ->
      let query = "map : 'f list * ('f -> 'g)  -> 'g list"
      and path = "List.map"
      and candidate = "('a -> 'b) -> 'a list -> 'b list" in
      let expected = 4
      and computed =
        Query.(
          distance_for (from_string query) ~path
            (candidate |> Type_expr.from_string |> Option.get))
      in
      check int "should be equal" expected computed)

let test_distance_7 =
  let open Alcotest in
  test_case "test distance from a query - 7" `Quick (fun () ->
      let query = "List : 'f list -> ('f -> 'g)  -> 'g list"
      and path = "List.map"
      and candidate = "('a -> 'b) -> 'a list -> 'b list" in
      let expected = 1
      and computed =
        Query.(
          distance_for (from_string query) ~path
            (candidate |> Type_expr.from_string |> Option.get))
      in
      check int "should be equal" expected computed)

let test_distance_8 =
  let open Alcotest in
  test_case "test distance from a query - 8" `Quick (fun () ->
      let query = "string -> int option"
      and path = "List.map"
      and candidate = "('a -> 'b) -> 'a list -> 'b list" in
      let expected = 1000
      and computed =
        Query.(
          distance_for (from_string query) ~path
            (candidate |> Type_expr.from_string |> Option.get))
      in
      check int "should be equal" expected computed)

let cases =
  ( "query-parser",
    [ test_distance_1;
      test_distance_2;
      test_distance_3;
      test_distance_4;
      test_distance_5;
      test_distance_6;
      test_distance_7;
      test_distance_8
    ] )
