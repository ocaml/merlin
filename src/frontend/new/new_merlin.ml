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
  List.iter (fun (New_commands.Command (name, doc, _, _, _)) ->
      prerr_string ("  " ^ name ^ "\n\t");
      prerr_endline doc
    ) New_commands.all_commands

let run = function
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
    match New_commands.find_command query New_commands.all_commands with
    | exception Not_found ->
      prerr_endline ("Unknown command " ^ query ^ ".\n");
      usage ();
      exit 1
    | New_commands.Command (_name, doc, spec, command_args, command_action) ->
      let notifications = ref [] in
      Logger.with_notifications notifications @@ fun () ->
      match begin
        let config, command_args =
          Marg.parse_all ~warning:prerr_endline
            Mconfig.arguments_table spec
            raw_args Mconfig.initial command_args
        in
        let config = Mconfig.(match config.query.directory with
            | "" -> config
            | dir ->
              let merlin = config.merlin in
              let merlin = {merlin with dotmerlin_to_load =
                                          dir :: merlin.dotmerlin_to_load} in
              {config with merlin}
          )
        in
        let trace = Trace.start () in
        let source = Msource.make config (Misc.string_of_file stdin) in
        let json =
          let class_, message =
            match command_action (trace,config,source) command_args with
            | result ->
              ("return", result)
            | exception (Failure str) ->
              ("failure", `String str)
            | exception exn ->
              ("exception", `String (Printexc.to_string exn))
          in
          let notify (sec,str) = `String (Printf.sprintf "%s: %s" sec str) in
          `Assoc ["class", `String class_; "value", message;
                  "notifications",
                  `List (List.rev_map notify !notifications)];
        in
        begin match Mconfig.(config.merlin.protocol) with
          | `Sexp -> Sexp.tell_sexp print_string (Sexp.of_json json)
          | `Json -> Std.Json.to_channel stdout json
        end;
        print_newline ()
      end with
      | () -> exit 0
      | exception exn ->
        prerr_endline ("Exception: " ^ Printexc.to_string exn);
        exit 1
