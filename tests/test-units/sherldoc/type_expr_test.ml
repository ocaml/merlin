open Merlin_sherlodoc

let type_testable =
  let pp ppf x = Format.fprintf ppf "%s" (Type_expr.to_string x) in
  Alcotest.testable pp Type_expr.equal

let test_parse_simple_type_1 =
  let open Alcotest in
  test_case "parse a simple type expression - 1" `Quick (fun () ->
      let expected = Some Type_expr.(Tycon ("int", []))
      and computed = Type_expr.from_string "int" in
      check (option type_testable) "should be an integer" expected computed)

let test_parse_simple_type_2 =
  let open Alcotest in
  test_case "parse a simple type expression - 2" `Quick (fun () ->
      let expected = Some Type_expr.(Tycon ("Result.t", [ Tyvar 0; Tyvar 1 ]))
      and computed = Type_expr.from_string "('foo, 'bar) Result.t" in
      check (option type_testable) "should be a result" expected computed)

let test_parse_simple_type_3 =
  let open Alcotest in
  test_case "parse a simple type expression - 3" `Quick (fun () ->
      let expected =
        Some
          Type_expr.(
            Arrow
              ( Arrow (Tyvar 0, Tyvar 1),
                Arrow (Tycon ("list", [ Tyvar 0 ]), Tycon ("list", [ Tyvar 1 ]))
              ))
      and computed = Type_expr.from_string "('a -> 'b) -> 'a list -> 'b list" in
      check (option type_testable) "should be the map function" expected
        computed)

let test_parse_simple_type_4 =
  let open Alcotest in
  test_case "parse a simple type expression - 4" `Quick (fun () ->
      let expected = Some Type_expr.(Arrow (Wildcard, Tycon ("Foo.bar", [])))
      and computed = Type_expr.from_string "_ -> Foo.bar" in
      check (option type_testable) "should be a simple query" expected computed)

let test_simple_isomorphismic_poly_function_1 =
  let open Alcotest in
  test_case
    "ensure that function equivalent function are parsed as the same function \
     - 1"
    `Quick (fun () ->
      let expected = Type_expr.from_string "('a -> 'b) -> 'a list -> 'b list"
      and computed =
        Type_expr.from_string "('foo -> 'bar) -> 'foo list -> 'bar list"
      in
      check (option type_testable) "should be equal" expected computed)

let test_poly_identifier_1 =
  let open Alcotest in
  test_case "recompute type variables - 1" `Quick (fun () ->
      let expected =
        Some
          "'a -> 'b -> 'a -> 'c -> 'd -> int -> ('a * 'c * string * 'b * 'c * \
           ('a, 'b) result) -> 'd t"
      and computed =
        "'foo -> 'bar -> 'foo -> 'baz -> 'rk -> int -> 'foo * 'baz * string * \
         'bar * 'baz * ('foo, 'bar) result -> 'rk t" |> Type_expr.from_string
        |> Option.map Type_expr.to_string
      in
      check (option string) "should be equal" expected computed)

let test_long_poly_identifier_1 =
  let open Alcotest in
  test_case "check polymorphic variable identifier generation - 1" `Quick
    (fun () ->
      let expected =
        Some
          "'a -> 'b -> 'c -> 'b -> 'c -> 'c -> 'b -> 'd -> 'e -> 'f -> 'g -> \
           'h -> 'i -> 'j -> int -> float -> 'k -> 'l -> 'm -> 'n -> 'o -> 'p \
           -> 'q -> 'r option -> 'b -> 's -> 't -> 'u -> 'a Option.t -> ('b, \
           'c) Result.t -> 'a -> 'r -> 'v -> 'd -> 'e -> 'w -> 'f -> 'g -> 'x \
           -> 'y -> 'z -> 'aa -> 'bb -> 'cc -> 'dd -> 'ee -> 'ff -> 'gg -> 'hh \
           -> 'ii -> 'jj -> 'kk -> 'll -> 'mm -> 'nn -> 'oo -> 'pp -> 'qq -> \
           'rr -> 'ss -> 'tt -> 'uu -> 'vv -> 'ww -> 'xx -> 'yy -> 'zz -> 'aaa \
           -> 'bbb -> 'ccc -> 'ddd -> 'eee -> 'fff -> 'ggg -> 'hhh -> 'k -> \
           'iii -> 'jjj -> 'kkk -> 'lll -> 'mmm -> 'nnn -> 'ooo -> 'ppp -> \
           'qqq -> 'rrr -> 'n -> 'sss -> 'ttt -> 'uuu -> 'vvv -> 'www -> 'o -> \
           'xxx -> 'yyy -> 'zzz -> 'aaaa -> 'bbbb -> 'cccc -> 'dddd -> 'eeee \
           -> 'l -> 'ffff -> 'gggg -> 'hhhh -> 'iiii -> 'jjjj -> 'kkkk -> \
           'llll -> 'mmmm -> 'nnnn -> 'oooo -> 'pppp -> 'p -> 'qqqq -> 'rrrr \
           -> 'ssss -> 'tttt -> 'uuuu -> 'vvvv -> 'wwww -> 'xxxx -> 'yyyy -> \
           'zzzz -> 'aaaaa -> 'bbbbb -> 'ccccc -> 'm -> 'ddddd -> 'eeeee -> \
           'fffff -> 'ggggg -> 'hhhhh -> 'iiiii -> 'jjjjj -> 'kkkkk -> 'lllll \
           -> 'mmmmm -> 'nnnnn -> 'ooooo -> 'ppppp -> 'qqqqq -> 'rrrrr -> \
           'sssss -> 'ttttt -> 'uuuuu -> 'vvvvv -> 'wwwww -> 'xxxxx -> 'yyyyy \
           -> 'zzzzz -> 'aaaaaa -> 'bbbbbb -> 'cccccc -> 'dddddd -> 'eeeeee -> \
           'ffffff -> 'gggggg -> 'hhhhhh -> 'iiiiii -> 'jjjjjj -> 'kkkkkk -> \
           'llllll -> 'mmmmmm -> 'nnnnnn -> 'oooooo -> 'pppppp -> 'qqqqqq -> \
           'rrrrrr -> 'ssssss -> 'tttttt -> 'uuuuuu -> 'vvvvvv -> 'wwwwww -> \
           'xxxxxx -> 'yyyyyy -> 'zzzzzz -> 'aaaaaaa -> 'bbbbbbb -> 'ccccccc \
           -> 'ddddddd -> 'eeeeeee -> 'fffffff -> 'ggggggg -> 'hhhhhhh -> \
           'iiiiiii -> 'jjjjjjj -> 'kkkkkkk -> 'lllllll -> 'mmmmmmm -> \
           'nnnnnnn -> 'ooooooo -> 'ppppppp -> 'qqqqqqq -> 'rrrrrrr -> \
           'sssssss -> 'ttttttt -> 'uuuuuuu -> 'vvvvvvv -> 'wwwwwww -> \
           'xxxxxxx -> 'yyyyyyy -> 'zzzzzzz -> 'aaaaaaaa -> 'bbbbbbbb -> \
           'cccccccc -> 'dddddddd -> 'eeeeeeee -> 'ffffffff -> 'gggggggg -> 'g"
      and computed =
        "'a -> 'foo -> 'bar -> 'foo -> 'bar -> 'bar -> 'foo -> 'd -> 'e -> 'g \
         -> 'h -> 't1 -> 't3 -> 't4 -> int -> float -> 'tt -> 'ttt -> 'tttt -> \
         'eee -> 'kkk -> 'ffff -> 'aq -> 'b option -> 'foo -> 'aaaaaaaa -> 'f2 \
         -> 'f3 -> 'a Option.t -> ('foo, 'bar) Result.t -> 'a -> 'b -> 'c -> \
         'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'k -> 'l -> 'm -> 'n -> 'o \
         -> 'p -> 'q -> 'r -> 's -> 't -> 'u -> 'v -> 'w -> 'x -> 'y -> 'z -> \
         'aa -> 'bb -> 'cc -> 'dd -> 'ee -> 'ff -> 'gg -> 'hh -> 'ii -> 'jj -> \
         'kk -> 'll -> 'mm -> 'nn -> 'oo -> 'pp -> 'qq -> 'rr -> 'ss -> 'tt -> \
         'uu -> 'vv -> 'ww -> 'xx -> 'yy -> 'zz -> 'aaa -> 'bbb -> 'ccc -> \
         'ddd -> 'eee -> 'fff -> 'ggg -> 'hhh -> 'iii -> 'jjj -> 'kkk -> 'lll \
         -> 'mmm -> 'nnn -> 'ooo -> 'ppp -> 'qqq -> 'rrr -> 'sss -> 'ttt -> \
         'uuu -> 'vvv -> 'www -> 'xxx -> 'yyy -> 'zzz -> 'aaaa -> 'bbbb -> \
         'cccc -> 'dddd -> 'eeee -> 'ffff -> 'gggg -> 'hhhh -> 'iiii -> 'jjjj \
         -> 'kkkk -> 'llll -> 'mmmm -> 'nnnn -> 'oooo -> 'pppp -> 'qqqq -> \
         'rrrr -> 'ssss -> 'tttt -> 'uuuu -> 'vvvv -> 'wwww -> 'xxxx -> 'yyyy \
         -> 'zzzz -> 'aaaaa -> 'bbbbb -> 'ccccc -> 'ddddd -> 'eeeee -> 'fffff \
         -> 'ggggg -> 'hhhhh -> 'iiiii -> 'jjjjj -> 'kkkkk -> 'lllll -> 'mmmmm \
         -> 'nnnnn -> 'ooooo -> 'ppppp -> 'qqqqq -> 'rrrrr -> 'sssss -> 'ttttt \
         -> 'uuuuu -> 'vvvvv -> 'wwwww -> 'xxxxx -> 'yyyyy -> 'zzzzz -> \
         'aaaaaa -> 'bbbbbb -> 'cccccc -> 'dddddd -> 'eeeeee -> 'ffffff -> \
         'gggggg -> 'hhhhhh -> 'iiiiii -> 'jjjjjj -> 'kkkkkk -> 'llllll -> \
         'mmmmmm -> 'nnnnnn -> 'oooooo -> 'pppppp -> 'qqqqqq -> 'rrrrrr -> \
         'ssssss -> 'tttttt -> 'uuuuuu -> 'vvvvvv -> 'wwwwww -> 'xxxxxx -> \
         'yyyyyy -> 'zzzzzz -> 'aaaaaaa -> 'bbbbbbb -> 'ccccccc -> 'ddddddd -> \
         'eeeeeee -> 'fffffff -> 'ggggggg -> 'hhhhhhh -> 'iiiiiii -> 'jjjjjjj \
         -> 'kkkkkkk -> 'lllllll -> 'mmmmmmm -> 'nnnnnnn -> 'ooooooo -> \
         'ppppppp -> 'qqqqqqq -> 'rrrrrrr -> 'sssssss -> 'ttttttt -> 'uuuuuuu \
         -> 'vvvvvvv -> 'wwwwwww -> 'xxxxxxx -> 'h" |> Type_expr.from_string
        |> Option.map Type_expr.to_string
      in
      check (option string) "should be equal" expected computed)

let cases =
  ( "type_expr",
    [ test_parse_simple_type_1;
      test_parse_simple_type_2;
      test_parse_simple_type_3;
      test_parse_simple_type_4;
      test_simple_isomorphismic_poly_function_1;
      test_poly_identifier_1;
      test_long_poly_identifier_1
    ] )
