(** {1 Prepare command-line arguments} *)

let usage () =
  prerr_endline
    "Usage: ocamlmerlin command [options] -- [compiler flags]\n\
     Help commands are:\n\
    \  -version        Print version and exit\n\
    \  -vnum           Print version number and exit\n\
    \  -warn-help      Show description of warning numbers\n\
    \  -flags-help     Show description of accepted compiler flags\n\
    \  -commands-help  Describe all accepted commands\n"

let commands_help () =
  print_endline "Query commands are:";
  List.iter (fun (New_commands.Command (name, doc, args, _, _)) ->
      print_newline ();
      let args = List.map (fun (kind, (key0,desc,_)) ->
        let key1, desc =
          let len = String.length desc in
          match String.index desc ' ' with
          | 0 -> key0, String.sub desc 1 (len - 1)
          | idx -> key0 ^ " " ^ String.sub desc 0 idx,
                   String.sub desc (idx + 1) (len - idx - 1)
          | exception Not_found -> key0, desc
        in
        let key = match kind with
          | `Mandatory -> key1
          | `Optional  -> "[ " ^ key1 ^ " ]"
          | `Many      -> "[ " ^ key1 ^ " " ^ key0 ^ " ... ]"
        in
        key, (key1, desc)
      ) args in
      let args, descs = List.split args in
      print_endline ("### `" ^ String.concat " " (name :: args) ^ "`");
      print_newline ();
      let print_desc (k,d) = print_endline (Printf.sprintf "% 24s  %s" k d) in
      List.iter print_desc descs;
      print_newline ();
      print_endline doc
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
  | "-commands-help" :: _ ->
    commands_help ();
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
        let start_time = Misc.time_spent () in
        let config, command_args =
          let fails = ref [] in
          let config, command_args =
            Marg.parse_all ~warning:(fun fail -> fails := fail :: !fails)
              Mconfig.arguments_table (List.map snd spec)
              raw_args Mconfig.initial command_args
          in
          let config =
            Mconfig.({config with merlin = {config.merlin with failures = !fails}})
          in
          config, command_args
        in
        (* Start processing query *)
        Logger.with_log_file Mconfig.(config.merlin.log_file) @@ fun () ->
        Stat_cache.with_cache @@ fun () ->
        let tr = (if Mconfig.(config.merlin.trace) then
                    Trace.start () else Trace.null) in
        let source = Msource.make tr config (Misc.string_of_file stdin) in
        let pipeline = Mpipeline.make tr config source in
        let json =
          let class_, message =
            Printexc.record_backtrace true;
            match command_action pipeline command_args with
            | result ->
              ("return", result)
            | exception (Failure str) ->
              ("failure", `String str)
            | exception exn ->
              let trace = Printexc.get_backtrace () in
              Logger.log "New_merlin.run" "Command error backtrace" trace;
              match Location.error_of_exn exn with
              | None | Some `Already_displayed ->
                ("exception", `String (Printexc.to_string exn ^ "\n" ^ trace))
              | Some (`Ok err) ->
                Location.report_error Format.str_formatter err;
                ("error", `String (Format.flush_str_formatter ()))
          in
          let total_time = Misc.time_spent () -. start_time in
          let timing = Mpipeline.timing_information pipeline in
          let pipeline_time =
            List.fold_left (fun acc (_, k) -> k +. acc) 0.0 timing in
          let timing = ("total", total_time) ::
                       ("query", (total_time -. pipeline_time)) :: timing in
          let notify (sec,str) = `String (Printf.sprintf "%s: %s" sec str) in
          `Assoc [
            "class", `String class_; "value", message;
            "notifications", `List (List.rev_map notify !notifications);
            "timing", `Assoc (List.map (fun (k,v) -> k, `Float v) timing)
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
