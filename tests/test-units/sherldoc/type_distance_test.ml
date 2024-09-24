open Merlin_sherlodoc

let expected_distance query entry expected =
  let open Alcotest in
  test_case
    ("distance between `" ^ query ^ "` and `" ^ entry ^ "`")
    `Quick
    (fun () ->
      let query = query |> Type_expr.from_string |> Option.get in
      let entry = entry |> Type_expr.from_string |> Option.get in
      let computed = Type_distance.compute ~query ~entry in
      check int
        ("distance should be " ^ string_of_int expected)
        expected computed)

let cases =
  ( "type_distance",
    [ expected_distance "int" "int" 0;
      expected_distance "string" "string" 0;
      expected_distance "string -> int" "string -> int" 0;
      expected_distance "string -> int -> float" "string -> int -> float" 0;
      expected_distance "int -> srting -> float" "int -> string -> float" 2;
      expected_distance "('a -> 'b) -> 'a list -> 'b list"
        "('a -> 'b) -> 'a list -> 'b list" 0;
      expected_distance "('foo -> 'bar) -> 'foo list -> 'bar list"
        "('a -> 'b) -> 'a list -> 'b list" 0;
      expected_distance "'foo list -> ('foo -> 'bar) -> 'bar list"
        "('a -> 'b) -> 'a list -> 'b list" 0;
      expected_distance "foo -> bar -> baz" "int -> string" 1000;
      expected_distance "('a -> 'b) * 'a list -> 'b list"
        "('a -> 'b) -> 'a list -> 'b list" 3;
      expected_distance "'a * 'b -> 'b" "'a * 'b -> 'a" 1;
      expected_distance "'a * 'b -> 'a" "'a * 'b -> 'a" 0;
      expected_distance
        "'a -> 'b -> 'b -> 'a -> 'b -> 'c -> int -> string -> Bar.t -> 'b \
         option"
        "'foo -> 'bar -> 'bar -> 'foo -> 'bar -> 'baz -> foo -> Bar.t -> int \
         -> 'bar option"
        6;
      expected_distance "('a -> 'a) -> 'a list -> 'a list"
        "('a -> 'b) -> 'a list -> 'b list" 2;
      expected_distance "'a -> 'b option -> 'a option"
        "'b option -> 'a -> 'a option" 3
    ] )
