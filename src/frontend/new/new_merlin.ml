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


let with_env env f =
  let parseenv var =
    match String.index var '=' with
    | pos ->
      let key = String.sub var 0 pos in
      let value = String.sub var (pos + 1) (String.length var - pos - 1) in
      (key, Some value)
    | exception Not_found -> (var, None)
  in
  let getenv key =
    match Unix.getenv key with
    | value -> Some value
    | exception Not_found -> None
  in
  let setenv key = function
    | None -> Os_ipc.unsetenv key
    | Some value -> Unix.putenv key value
  in
  let rec setup = function
    | [] -> f ()
    | var :: vs ->
      let (key, value) = parseenv var in
      if key = "MERLIN_LOG" then
        Logger.with_log_file value (fun () -> setup vs)
      else
        let value' = getenv key in
        setenv key value;
        match setup vs with
        | exception exn ->
          setenv key value';
          Std.reraise exn
        | result ->
          setenv key value';
          result
  in
  setup env

let run env = function
  | [] ->
    usage ();
    1
  | "-version" :: _ ->
    Printf.printf "The Merlin toolkit version %s, for Ocaml %s\n"
      My_config.version Sys.ocaml_version;
    0
  | "-vnum" :: _ ->
    Printf.printf "%s\n" My_config.version;
    0
  | "-warn-help" :: _ ->
    Warnings.help_warnings ();
    0
  | "-flags-help" :: _ ->
    Mconfig.document_arguments stdout;
    0
  | query :: raw_args ->
    match New_commands.find_command query New_commands.all_commands with
    | exception Not_found ->
      prerr_endline ("Unknown command " ^ query ^ ".\n");
      usage ();
      1
    | New_commands.Command (_name, doc, spec, command_args, command_action) ->
      (* Setup notifications *)
      let notifications = ref [] in
      Logger.with_notifications notifications @@ fun () ->
      (* Parse commandline *)
      match begin
        let config, command_args =
          let fails = ref [] in
          let config, command_args =
            Marg.parse_all ~warning:(fun fail -> fails := fail :: !fails)
              Mconfig.arguments_table spec
              raw_args Mconfig.initial command_args
          in
          let config =
            let failures = !fails in
            Mconfig.({config with merlin = {config.merlin with failures}}) in
          let config = Mconfig.(match config.query.directory with
              | "" -> config
              | cwd ->
                let merlin = config.merlin in
                let path = Misc.canonicalize_filename ~cwd config.query.filename in
                let path =
                  let base = "." ^ Filename.basename path ^ ".merlin" in
                  Filename.concat (Filename.dirname path) base
                in
                let dotmerlin_to_load = path :: merlin.dotmerlin_to_load in
                let merlin = {merlin with dotmerlin_to_load} in
                {config with merlin}
            )
          in
          config, command_args
        in
        (* Start processing query *)
        Logger.with_log_file Mconfig.(config.merlin.log_file) @@ fun () ->
        Stat_cache.with_cache @@ fun () ->
        let tr = (if Mconfig.(config.merlin.trace) then
                    Trace.start () else Trace.null) in
        let source = Msource.make tr config (Misc.string_of_file stdin) in
        let json =
          let class_, message =
            match command_action (tr,config,source) command_args with
            | result ->
              ("return", result)
            | exception (Failure str) ->
              ("failure", `String str)
            | exception exn ->
              match Location.error_of_exn exn with
              | None -> ("exception", `String (Printexc.to_string exn))
              | Some err ->
                Location.report_error Format.str_formatter err;
                ("error", `String (Format.flush_str_formatter ()))
          in
          let notify (sec,str) = `String (Printf.sprintf "%s: %s" sec str) in
          `Assoc ["class", `String class_; "value", message;
                  "notifications",
                  `List (List.rev_map notify !notifications);
                 ];
        in
        begin match Mconfig.(config.merlin.protocol) with
          | `Sexp -> Sexp.tell_sexp print_string (Sexp.of_json json)
          | `Json -> Std.Json.to_channel stdout json
        end;
        print_newline ()
      end with
      | () -> 0
      | exception exn ->
        prerr_endline ("Exception: " ^ Printexc.to_string exn);
        1
