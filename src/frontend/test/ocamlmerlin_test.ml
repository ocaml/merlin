open Std

(* Poor man's test framework *)
type name = string

type test =
  | Single of name * (unit -> unit)
  | Group of name * test list

let test name f = Single (name, f)

let group name tests = Group (name, tests)

exception Detail of exn * string
let () = Printexc.register_printer (function
    | (Detail (exn, msg)) ->
      Some (Printexc.to_string exn ^ "\nAdditional information:\n" ^ msg)
    | _ -> None
  )

let str_match ~re str =
  Str.string_match (Str.regexp (re ^ "$")) str 0

(* Setting up merlin *)
module M = Mpipeline

let process ?(with_config=fun x -> x) ?for_completion filename text =
  let config = with_config Mconfig.initial in
  let config = Mconfig.({config with query = {config.query with filename}}) in
  let source = Msource.make Trace.null config text in
  let pipeline = M.make Trace.null config source in
  match for_completion with
  | None -> pipeline
  | Some pos -> M.for_completion pos pipeline

(* All tests *)

let assert_errors ?with_config
    filename ?(lexer=0) ?(parser=0) ?(typer=0) ?(config=0) source =
  test filename (fun () ->
      let m = process ?with_config filename source in
      let lexer_errors  = M.reader_lexer_errors m in
      let parser_errors = M.reader_parser_errors m in
      let failures, typer_errors  =
        Mtyper.with_typer (M.typer_result m) @@ fun () ->
        Mconfig.((M.final_config m).merlin.failures),
        M.typer_errors m
      in
      let fmt_msg exn =
        match Location.error_of_exn exn with
        | None | Some `Already_displayed -> Printexc.to_string exn
        | Some (`Ok err) -> err.Location.msg
      in
      let expect ~count str errors =
        let count' = List.length errors in
        if count <> count' then failwith (
            "expecting " ^ string_of_int count ^ " " ^ str ^ " but got " ^
            string_of_int count' ^ " errors\n" ^
            String.concat "\n- " ("Errors: " :: List.map_end fmt_msg
                                    (lexer_errors @ parser_errors @ typer_errors)
                                    failures)
          )
      in
      expect ~count:lexer "lexer errors" lexer_errors;
      expect ~count:parser "parser errors" parser_errors;
      expect ~count:typer "typer errors" typer_errors;
      expect ~count:config "configuration failures" failures;
    )

let assertf b fmt =
  if b then
    Printf.ikfprintf ignore () fmt
  else
    Printf.ksprintf failwith fmt

let validate_output ?with_config filename source query pred =
  test filename (fun () ->
      let pipeline = process ?with_config filename source in
      let result = Query_commands.dispatch pipeline query in
      try pred result
      with exn ->
        let info = `Assoc [
            "query", Query_json.dump query;
            "result", Query_json.json_of_response query result;
          ] in
        raise (Detail (exn, Json.pretty_to_string info))
    )

(* FIXME: this sucks. improve. *)
let validate_failure ?with_config filename source query pred =
  test filename (fun () ->
      let pipeline = process ?with_config filename source in
      let for_info, wrapped =
        match Query_commands.dispatch pipeline query with
        | exception e -> ("failure", `String (Printexc.to_string e)), `Error e
        | res -> ("result", Query_json.json_of_response query res), `Ok res
      in
      try pred wrapped
      with exn ->
        let info = `Assoc [ "query", Query_json.dump query; for_info ] in
        raise (Detail (exn, Json.pretty_to_string info))
    )

let tests = [

  group "no-escape" (
    [
      (* These tests ensure that all type errors are caught by the kernel,
         no exception should reach top-level *)

      assert_errors "incorrect_gadt.ml"
        ~parser:1 ~typer:1
        "type p = P : 'a -> 'a -> p";

      assert_errors "unkown_constr.ml"
        ~typer:1
        "let error : unknown_type_constructor = assert false";

      assert_errors "unkown_constr.mli"
        ~typer:1
        "val error : unknown_type_constructor";

      assert_errors "two_constr.ml"
        ~typer:1
        "type t = A | A\n";

      assert_errors "two_constr.mli"
        ~typer:1
        "type t = A | A\n";

      assert_errors "ml_in_mli.mli"
        ~parser:1
        "let x = 4 val x : int";

      assert_errors "mli_in_ml.ml"
        ~typer:1 (* vals are no allowed in ml files and detected
                       during semantic analysis *)
        "val x : int";

      assert_errors "unused_case_after_error.ml"
        ~typer:1 (* The code should raise a single error (for Bb typo),
                    but shouldn't report the unused case after *)
        "type t = A | B | C\n\
         let f = function A -> 1 | Bb -> 1 | C -> 1";
    ]
  );

  group "recovery" [
    assert_errors "hole_0.ml"
      (* ?? should be parsed as merlin.hole, and merlin.hole shouldn't be
         treated as a type error. *)
      "let () = ??";
    assert_errors "hole_1.ml"
      ~parser:1 (* This incomplete expression should generate only a parser
                      error. The hole is filled with merlin.hole. *)
      "let _ =";
    assert_errors "hole_2.ml"
      ~parser:1 (* A bit trickier: the recovery is tempted to put a ->.
                      (unreachable), but the penalty should prevent it.  *)
      "let _ = function _ ->";
    assert_errors "nothing_to_recover.ml"
      ~parser:1 (* Issue #713: merlin would error when it cannot recover,
                   but in some files there really is nothing to recover. *)
      "let";
  ];

  group "ocaml-flags" (
    let assert_errors ?lexer ?parser ?typer ?(flags=[]) filename source =
      assert_errors ?lexer ?parser ?typer
        ~with_config:(fun config ->
            let flags = {
              Mconfig.
              flag_cwd = None;
              flag_list = flags;
            } in
            Mconfig.({config with merlin = {config.merlin with
                                            flags_to_apply = [flags]}}))
        filename
        source
    in
    [

      (* -unsafe and array desugaring *)

      assert_errors "array_good.ml"
        "let x = [|0|].(0)";

      assert_errors "array_bad.ml"
        ~typer:1
        "module Array = struct end\n\
         let x = [|0|].(0)";

      assert_errors "array_fake_good.ml"
        "module Array = struct let get _ _ = () end\n\
         let x = [|0|].(0)";

      assert_errors ~flags:["-unsafe"] "unsafe_array_good.ml"
        "let x = [|0|].(0)";

      assert_errors ~flags:["-unsafe"] "unsafe_array_bad.ml"
        ~typer:1
        "module Array = struct end\n\
         let x = [|0|].(0)";

      assert_errors ~flags:["-unsafe"] "unsafe_array_fake_good.ml"
        "module Array = struct let unsafe_get _ _ = () end\n\
         let x = [|0|].(0)";

      (* classic and labels *)

      assert_errors "labels_ok_1.ml"
        "let f ~x = () in f ~x:(); f ()";

      assert_errors ~flags:["-nolabels"] "classic_ko_1.ml"
        "let f ~x = () in f ~x:(); f ()";
    ]
  );

  group "path-expansion" (
    let test_ppx_path name flag_list ?cwd item =
      test name (fun () ->
          let open Mconfig in
          let m = process ~with_config:(fun cfg ->
              let merlin = {cfg.merlin with
                            flags_to_apply = [{flag_cwd = cwd; flag_list}]} in
              {cfg with merlin}
            ) "relative_path.ml" ""
          in
          let config = Mpipeline.reader_config m in
          let dump () cfg = Json.pretty_to_string (Mconfig.dump cfg) in
          assertf
            (List.mem item config.ocaml.ppx)
            "Expecting %s in config.\nConfig:\n%a"
            item dump config;
        )
    in
    let cwd = Filename.get_temp_dir_name () in
    [
      (* Simple name is not expanded *)
      test_ppx_path "simple_name" ["-ppx"; "test1"] "test1";

      (* Absolute name is not expanded *)
      test_ppx_path "absolute_path" ["-ppx"; "/test2"] "/test2";

      (* Relative name is expanded *)
      test_ppx_path "relative_path" ~cwd
        ["-ppx"; "./test3"] (Filename.concat cwd "test3");

      (* Quoted flags inherit path *)
      test_ppx_path "quoted_path" ~cwd
        ["-flags"; "-ppx ./test4"] (Filename.concat cwd "test4");
    ]

  );

  group "destruct" (
    [
      (* TODO: test all error cases. *)

      validate_failure "nothing_to_do.ml"
        "let _ = match (None : unit option) with None -> () | Some () -> ()"
        (Query_protocol.Case_analysis (`Offset 58, `Offset 60))
        (function
          | `Error Destruct.Nothing_to_do -> ()
          | _  -> assertf false "expected Nothing_to_do exception");

      (* TODO: at some point properly check locations as well. *)

      validate_output "make_exhaustive.ml"
        "let _ = match (None : unit option) with None -> ()"
        (Query_protocol.Case_analysis (`Offset 40, `Offset 44))
        (fun (_loc, s) ->
           let expected = "\n| Some _ -> (??)" in
           assertf (s = expected) "expected %S" expected);

      validate_output "refine_pattern.ml"
        "let _ = match (None : unit option) with None -> ()\n| Some _ -> (??)"
        (Query_protocol.Case_analysis (`Offset 59, `Offset 60))
        (fun (_loc, s) ->
           let expected = "()" in
           assertf (s = expected) "expected %S" expected);

      validate_output "unpack_module.ml"
        "module type S = sig end\n\nlet g (x : (module S)) =\n  x"
        (Query_protocol.Case_analysis (`Offset 52, `Offset 53))
        (fun (_loc, s) ->
           let expected = "let module M = (val x) in (??)" in
           assertf (s = expected) "expected %S" expected);

      validate_output "record_exp.ml"
        "let f (x : int ref) =\n  x"
        (Query_protocol.Case_analysis (`Offset 24, `Offset 25))
        (fun (_loc, s) ->
           let expected = "match x with | { contents } -> (??)" in
           assertf (s = expected) "expected %S" expected);

      validate_output "variant_exp.ml"
        "let f (x : int option) =\n  x"
        (Query_protocol.Case_analysis (`Offset 27, `Offset 28))
        (fun (_loc, s) ->
           let expected = "match x with | None -> (??) | Some _ -> (??)" in
           assertf (s = expected) "expected %S" expected);

    ]
  );

  group "misc" (
    [
      assert_errors "relaxed_external.ml"
        "external test : unit = \"bs\"";

      validate_output "occurrences.ml"
        "let foo _ = ()\nlet () = foo 4\n"
        (Query_protocol.Occurrences (`Ident_at (`Offset 5)))
        (fun locations ->
           assertf (List.length locations = 2) "expected two locations");

      validate_output "locate.ml"
        "let foo _ = ()\nlet () = foo 4\n"
        (Query_protocol.Locate (None, `ML, `Offset 26))
        (function
          | `Found (Some "locate.ml", pos)
            when Lexing.split_pos pos = (1, 4) -> ()
          | _ -> assertf false "Expecting match at position (1, 4)");

      assert_errors "invalid_flag.ml" ~config:1
        ~with_config:(fun cfg ->
            let open Mconfig in
            let flags_to_apply = [{
                flag_cwd = None;
                flag_list = ["-lalala"]
              }] in
            Mconfig.({cfg with merlin = {cfg.merlin with flags_to_apply}}))
         ""
      ;
    ]
  );

  group "type-expr" (

    let test_file =
      "let x = 5\n\
       let y = 10\n
       type t = T\n\
       module M = List\n\
       module type MT = module type of List\n\
       let z = ()"
    in
    let unbound_pattern = `Start, "\\(Error: \\)?Unbound .*" in
    let queries = [
      "lident-value"       , "y"     , [unbound_pattern; `End, "int"];
      "lident-type"        , "t"     , [unbound_pattern; `End, "type t = T"];
      "expr"               , "x + y" , [unbound_pattern; `End, "int"];
      "uident-constructor" , "T"     , [unbound_pattern; `End, "t"];
      "uident-module"      , "M"     , [unbound_pattern; `End, "(module List)"];
      "uident-module-type" , "MT"    , [unbound_pattern; `End, "sig\\(.\\|\n\\)*end"];
      "parse-error"        , "f ("   , [`Start, "FIXME"];
    ] in
    let type_expr_match name expr pos re =
      validate_output name test_file (Query_protocol.Type_expr (expr, pos))
        (fun str -> assertf (str_match ~re str)
            "Output didn't match pattern %S: %S" re str)
    in
    List.concat_map queries ~f:(fun (name, expr, patterns) ->
        List.map patterns ~f:(fun (pos, re) ->
            type_expr_match (name ^ "-" ^ Msource.print_position () pos) expr pos re))
  );

  group "motion" (
    let check_position (l1,c1 as p1) pos =
      let l2,c2 as p2 = Lexing.split_pos pos in
      assertf (p1=p2) "Expecting to move to %d:%d, moved to %d:%d" l1 c1 l2 c2
    in
    let check_jump_position a b = match a, b with
      | None, `Error _ -> ()
      | None, `Found pos ->
        let l, c = Lexing.split_pos pos in
        assertf false "Expected to fail, moved to %d:%d" l c
      | Some (l, c), `Error msg ->
        assertf false
          "Expected to move to %d:%d, but failed with message %S" l c msg
      | Some pos1, `Found pos2 -> check_position pos1 pos2
    in
    let jump ?with_config ~from ?result feature source id =
      let name = match result with
        | Some (l,c) -> sprintf "jump_%d_to_%s_at_%d_%d.ml" id feature l c
        | None -> sprintf "jump_%d_to_%s_fail.ml" id feature
      in
      validate_output ?with_config name source
        (Query_protocol.Jump (feature, `Logical from))
        (check_jump_position result)
    in
    let phrase ?with_config ~from ~result direction source id =
      let (l,c) = result in
      let name = sprintf "phrase_%s_%d_from_%d_%d.ml"
          (if direction = `Next then "next" else "prev") id l c in
      validate_output ?with_config name source
        (Query_protocol.Phrase (direction, `Logical from))
        (check_position result)
    in
    List.mapi ~f:(fun i f -> f i) [
      jump "let" ~from:(2,2) ~result:(1,0) "let x =\n  5";
      jump "let" ~from:(1,8) "let x = 5"; (*Same line should fail*)
      jump "module" ~from:(2,2) "let x = \n 5";
      phrase `Next ~from:(1,0) "let x = 5\nlet y = 2" ~result:(2,0);
      phrase `Prev ~from:(2,0) "let x = 5\nlet y = 2" ~result:(1,0);
    ]
  );

  group "std" [

    group "glob" (
      let glob_match ~pattern str =
        Glob.match_pattern (Glob.compile_pattern pattern) str in
      let should_match name ~pattern str =
        test name (fun () -> assertf (glob_match ~pattern str)
                      "pattern %S should match %S" pattern str)
      and shouldn't_match name ~pattern str =
        test name (fun () -> assertf (not (glob_match ~pattern str))
                      "pattern %S shouldn't match %S" pattern str)
      in
      [
        should_match "empty" ~pattern:"" "";
        shouldn't_match "not-empty" ~pattern:"" "x";
        should_match "litteral" ~pattern:"x" "x";
        shouldn't_match "not-litteral" ~pattern:"x" "y";
        should_match "skip" ~pattern:"x?z" "xyz";
        shouldn't_match "not-skip" ~pattern:"x?yz" "xyz";
        should_match "joker1" ~pattern:"x*" "xyz";
        shouldn't_match "not-joker1" ~pattern:"y*" "xyz";
        should_match "joker2" ~pattern:"xy*xy*" "xyzxyz";
        shouldn't_match "not-joker2" ~pattern:"xy*yz*" "xyzyxz";
        should_match "joker3" ~pattern:"*bar*" "foobarbaz";
      ]
    );

    group "shell" (
      let string_list = function
        | [] -> "[]"
        | comps ->
          let comps = List.map ~f:String.escaped comps in
          "[\"" ^ String.concat ~sep:"\";\"" comps ^ "\"]"
      in
      let assert_split i (str, expected) =
        test ("split_command-" ^ string_of_int i) @@ fun () ->
        let result = Shell.split_command str in
        assertf (result = expected)
          "Shell.split_command %S = %s, expecting %s"
          str (string_list result) (string_list expected)
      in
      List.mapi ~f:assert_split [
        "a b c"     , ["a";"b";"c"];
        "a'b'c"     , ["abc"];
        "a 'b c'"   , ["a"; "b c"];
        "a\"b'c\""  , ["ab'c"];
        "a\\\"b'c'" , ["a\"bc"];
      ]
    );
  ];
]

(* Driver *)

let passed = ref 0
let failed = ref 0

let rec run_tests indent = function
  | [] -> ()
  | x :: xs ->
    run_test indent x;
    run_tests indent xs

and run_test indent = function
  | Single (name, f) ->
    Printf.printf "%s%s:\t%!" indent name;
    begin match f () with
      | () ->
        incr passed;
        Printf.printf "OK\n%!"
      | exception exn ->
        let bt = Printexc.get_backtrace () in
        incr failed;
        Printf.printf "KO\n%!";
        Printf.eprintf "%sTest %s failed with exception:\n%s%s\n%!"
          indent name
          indent
          (match exn with
           | Failure str -> str
           | exn -> Printexc.to_string exn);
        begin match Location.error_of_exn exn with
          | None | Some `Already_displayed -> ()
          | Some (`Ok {Location. msg; loc}) ->
            Printf.eprintf "%sError message:\n%s\n%!" indent msg
        end;
        Printf.eprintf "%sBacktrace:\n%s\n%!" indent bt
    end
  | Group (name, tests) ->
    Printf.printf "%s-> %s\n" indent name;
    run_tests (indent ^ "  ") tests

let () =
  Printexc.record_backtrace true;
  run_tests "  " tests;
  Printf.printf "Passed %d, failed %d\n" !passed !failed;
  if !failed > 0 then exit 1
