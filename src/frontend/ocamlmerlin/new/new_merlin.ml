(** {1 Prepare command-line arguments} *)

let { Logger.log } = Logger.for_section "New_merlin"

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
  List.iter
    (fun (New_commands.Command (name, doc, args, _, _)) ->
      print_newline ();
      let args =
        List.map
          (fun (kind, (key0, desc, _)) ->
            let key1, desc =
              let len = String.length desc in
              match String.index desc ' ' with
              | 0 -> (key0, String.sub desc 1 (len - 1))
              | idx ->
                ( key0 ^ " " ^ String.sub desc 0 idx,
                  String.sub desc (idx + 1) (len - idx - 1) )
              | exception Not_found -> (key0, desc)
            in
            let key =
              match kind with
              | `Mandatory -> key1
              | `Optional -> "[ " ^ key1 ^ " ]"
              | `Many -> "[ " ^ key1 ^ " " ^ key0 ^ " ... ]"
            in
            (key, (key1, desc)))
          args
      in
      let args, descs = List.split args in
      print_endline ("### `" ^ String.concat " " (name :: args) ^ "`");
      print_newline ();
      let print_desc (k, d) = print_endline (Printf.sprintf "%24s  %s" k d) in
      List.iter print_desc descs;
      print_newline ();
      print_endline doc)
    New_commands.all_commands

let run =
  let query_num = ref (-1) in
  function
  | [] ->
    usage ();
    1
  | "-version" :: _ ->
    Printf.printf "The Merlin toolkit version %s, for Ocaml %s\n"
      Merlin_config.version Sys.ocaml_version;
    0
  | "-vnum" :: _ ->
    Printf.printf "%s\n" Merlin_config.version;
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
  | query :: raw_args -> (
    incr query_num;
    match New_commands.find_command query New_commands.all_commands with
    | exception Not_found ->
      prerr_endline ("Unknown command " ^ query ^ ".\n");
      usage ();
      1
    | New_commands.Command (_name, _doc, spec, command_args, command_action)
      -> (
      (* Setup notifications *)
      let notifications = ref [] in
      Logger.with_notifications notifications @@ fun () ->
      (* Parse commandline *)
      match
        begin
          let start_cpu = Misc.time_spent () in
          let start_clock = Unix.gettimeofday () *. 1000. in
          let config, command_args =
            let fails = ref [] in
            let config, command_args =
              Mconfig.parse_arguments ~wd:(Sys.getcwd ())
                ~warning:(fun w -> fails := w :: !fails)
                (List.map snd spec) raw_args Mconfig.initial command_args
            in
            let config =
              let failures = !fails @ config.merlin.failures in
              Mconfig.{ config with merlin = { config.merlin with failures } }
            in
            (config, command_args)
          in
          (* Start processing query *)
          Logger.with_log_file
            Mconfig.(config.merlin.log_file)
            ~sections:Mconfig.(config.merlin.log_sections)
          @@ fun () ->
          Mocaml.flush_caches
            ~older_than:
              (float_of_int (60 * Mconfig.(config.merlin.cache_lifespan)))
            ();
          File_id.with_cache @@ fun () ->
          let source = Msource.make (Misc.string_of_file stdin) in
          let pipeline = Mpipeline.make config source in
          let json =
            let class_, message =
              Printexc.record_backtrace true;
              match
                Mpipeline.with_pipeline pipeline @@ fun () ->
                command_action pipeline command_args
              with
              | result -> ("return", result)
              | exception Failure str ->
                let trace = Printexc.get_backtrace () in
                log ~title:"run" "Command error backtrace: %s" trace;
                ("failure", `String str)
              | exception exn -> (
                let trace = Printexc.get_backtrace () in
                log ~title:"run" "Command error backtrace: %s" trace;
                match Location.error_of_exn exn with
                | None | Some `Already_displayed ->
                  ("exception", `String (Printexc.to_string exn ^ "\n" ^ trace))
                | Some (`Ok err) ->
                  Location.print_main Format.str_formatter err;
                  ("error", `String (Format.flush_str_formatter ())))
            in
            let cpu_time = Misc.time_spent () -. start_cpu in
            let gc_stats = Gc.quick_stat () in
            let heap_mbytes =
              gc_stats.heap_words * (Sys.word_size / 8) / 1_000_000
            in
            let clock_time = (Unix.gettimeofday () *. 1000.) -. start_clock in
            let timing = Mpipeline.timing_information pipeline in
            let pipeline_time =
              List.fold_left (fun acc (_, k) -> k +. acc) 0.0 timing
            in
            let timing =
              ("clock", clock_time) :: ("cpu", cpu_time)
              :: ("query", cpu_time -. pipeline_time)
              :: timing
            in
            let notify { Logger.section; msg } =
              `String (Printf.sprintf "%s: %s" section msg)
            in
            let format_timing (k, v) = (k, `Int (int_of_float (0.5 +. v))) in
            `Assoc
              [ ("class", `String class_);
                ("value", message);
                ("notifications", `List (List.rev_map notify !notifications));
                ("timing", `Assoc (List.map format_timing timing));
                ("heap_mbytes", `Int heap_mbytes);
                ("cache", Mpipeline.cache_information pipeline);
                ("query_num", `Int !query_num)
              ]
          in
          log ~title:"run(result)" "%a" Logger.json (fun () -> json);
          begin
            match Mconfig.(config.merlin.protocol) with
            | `Sexp -> Sexp.tell_sexp print_string (Sexp.of_json json)
            | `Json -> Yojson.Basic.to_channel stdout json
          end;
          print_newline ()
        end
      with
      | () -> 0
      | exception exn ->
        prerr_endline ("Exception: " ^ Printexc.to_string exn);
        1))

let with_wd ~wd ~old_wd f args =
  match Sys.chdir wd with
  | () ->
    log ~title:"run" "changed directory to %S (old wd: %S)" wd old_wd;
    Fun.protect ~finally:(fun () -> Sys.chdir old_wd) (fun () -> f args)
  | exception Sys_error _ ->
    log ~title:"run" "cannot change working directory to %S (old wd: %S)" wd
      old_wd;
    f args

let run ~new_env wd args =
  begin
    match new_env with
    | Some env ->
      Os_ipc.merlin_set_environ env;
      Unix.putenv "__MERLIN_MASTER_PID" (string_of_int (Unix.getpid ()))
    | None -> ()
  end;
  let old_wd = Sys.getcwd () in
  let run args () =
    match wd with
    | Some wd -> with_wd ~wd ~old_wd run args
    | None ->
      log ~title:"run" "No working directory specified (old wd: %S)" old_wd;
      run args
  in
  let `Log_file_path log_file, `Log_sections sections = Log_info.get () in
  Logger.with_log_file log_file ~sections @@ run args
