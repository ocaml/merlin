(** {1 Prepare command-line arguments} *)

let usage () =
  prerr_endline
    "Usage: ocamlmerlin command [options] -- [compiler flags]\n\
     Help commands are:\n\
    \  -version     Print version and exit\n\
    \  -vnum        Print version number and exit\n\
    \  -warn-help   Show description of warning numbers\n\
    \  -flags-help  Show description of accepted compiler flags\n";
  prerr_endline "Query commands are:";
  List.iter (fun (Mqueries.Command (name, doc, _, _, _)) ->
      prerr_string ("  " ^ name ^ "\n\t");
      prerr_endline doc
    ) Mqueries.queries

let () =
  let arguments =
    match Array.to_list Sys.argv with
    | [] -> []
    | _ :: args -> args
  in
  match arguments with
  | [] ->
    usage ();
    exit 1
  | "-version" :: _ ->
    Printf.printf "The Merlin toolkit version %s, for Ocaml %s\n"
      My_config.version Sys.ocaml_version;
    exit 0
  | "-vnum" :: _ ->
    Printf.printf "%s\n" My_config.version;
    exit 0
  | "-warn-help" :: _ ->
    Warnings.help_warnings ()
  | "-flags-help" :: _ ->
    Mconfig.document_arguments stdout
  | query :: raw_args ->
    match Mqueries.find_command query Mqueries.queries with
    | exception Not_found ->
      prerr_endline ("Unknown command " ^ query ^ ".\n");
      usage ();
      exit 1
    | Mqueries.Command (_name, doc, spec, command_args, command_action) ->
      match begin
        let config, command_args =
          Marg.parse_all ~warning:prerr_endline
            Mconfig.arguments_table spec
            raw_args Mconfig.initial command_args
        in
        let trace = Trace.start () in
        let source =
          let text = Misc.string_of_file stdin in
          Msource.make ~filename:config.Mconfig.filename ~text
        in
        let reader = Mreader.make trace config source in
        let config, parsetree = Mppx.rewrite trace
            (Mreader.get_config reader) (Mreader.get_parsetree reader) in
        let input = { Mqueries. source; reader; config; parsetree } in
        Logger.logj "reference" "input" (fun () -> Mqueries.dump_input input);
        let json = command_action input command_args in
        Std.Json.pretty_to_channel stdout json;
        print_newline ()
      end with
      | () -> exit 0
      | exception exn ->
        prerr_endline ("Exception: " ^ Printexc.to_string exn);
        exit 1
