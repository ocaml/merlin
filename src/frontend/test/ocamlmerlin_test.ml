(* Poor man's test framework *)
type name = string

type test =
  | Single of name * (unit -> unit)
  | Group of name * test list

let test name f = Single (name, f)

let group name tests = Group (name, tests)

(* Setting up merlin *)
module M = Mpipeline

let from_source ?(with_config=fun x -> x) ~filename text =
  let config = with_config Mconfig.initial in
  let config = Mconfig.({config with query = {config.query with filename}}) in
  (config, Msource.make config text)

let process ?with_config ?for_completion filename text =
  let config, source = from_source ?with_config ~filename text in
  M.make ?for_completion (Trace.start ()) config source

(* All tests *)

let assert_errors ?with_config
    name ?(lexer=false) ?(parser=false) ?(typer=false) source =
  test name (fun () ->
      let m = process ?with_config name source in
      let no b = if not b then "no " else "" in
      if (M.reader_lexer_errors m <> []) <> lexer then
        failwith ("expected " ^ no lexer ^ "lexer errors");
      if (M.reader_parser_errors m <> []) <> parser then
        failwith ("expected " ^ no parser ^ "parser errors");
      Mtyper.with_typer (M.typer_result m) @@ fun () ->
      if (M.typer_errors m <> []) <> typer then
        failwith ("expected " ^ no typer ^ "typer errors");
    )

let tests = [

  group "no-escape" (
    [
      (* These tests ensure that all type errors are caught by the kernel,
         no exception should reach top-level *)

      assert_errors "incorrect_gadt.ml"
        ~parser:true ~typer:true
        "type p = P : 'a -> 'a -> p";

      assert_errors "unkown_constr.ml"
        ~typer:true
        "let error : unknown_type_constructor = assert false";

      assert_errors "unkown_constr.mli"
        ~typer:true
        "val error : unknown_type_constructor";

      assert_errors "ml_in_mli.mli"
        ~parser:true
        "let x = 4";

      assert_errors "mli_in_ml.ml"
        ~typer:true (* vals are no allowed in ml files and detected
                       during semantic analysis *)
        "val x : int";
    ]
  );

  group "ocaml-flags" (
    let assert_errors ?lexer ?parser ?typer ?(flags=[]) filename source =
      assert_errors ?lexer ?parser ?typer
        ~with_config:(fun config ->
            Mconfig.({config with merlin = {config.merlin with flags_to_apply = [flags]}}))
        filename
        source
    in
    [

      (* -unsafe and array desugaring *)

      assert_errors "array_good.ml"
        "let x = [|0|].(0)";

      assert_errors "array_bad.ml"
        ~typer:true
        "module Array = struct end\n\
         let x = [|0|].(0)";

      assert_errors "array_fake_good.ml"
        "module Array = struct let get _ _ = () end\n\
         let x = [|0|].(0)";

      assert_errors ~flags:["-unsafe"] "unsafe_array_good.ml"
        "let x = [|0|].(0)";

      assert_errors ~flags:["-unsafe"] "unsafe_array_bad.ml"
        ~typer:true
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
        incr failed;
        Printf.printf "KO\n%!";
        Printf.eprintf "%sTest %s failed with exception:\n%s%s\n%!"
          indent name
          indent (Printexc.to_string exn)
    end
  | Group (name, tests) ->
    Printf.printf "%s-> %s\n" indent name;
    run_tests (indent ^ "  ") tests

let () =
  run_tests "  " tests;
  Printf.printf "Passed %d, failed %d\n" !passed !failed;
  if !failed > 0 then exit 1
