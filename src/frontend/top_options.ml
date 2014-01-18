let print_version () =
  Printf.printf "The Merlin toolkit version %s, for Ocaml %s\n"
    My_config.version Sys.ocaml_version;
  exit 0

let print_version_num () =
  Printf.printf "%s\n" My_config.version;
  exit 0

let unexpected_argument s =
  failwith ("Unexpected argument: " ^ s)

module Make (Bootstrap : sig
               val _projectfind : string -> unit
               val _protocol : string -> unit
             end) = struct
  include Main_args.Make_top_options (struct
    let t = Clflags.t

    include Bootstrap

    let _debug section =
      match Misc.rev_string_split section ~on:',' with
      | [ section ] ->
        begin try Logger.(monitor (Section.of_string section))
        with Invalid_argument _ -> () end
      | [ log_path ; section ] ->
        begin try
          let section = Logger.Section.of_string section in
          Logger.monitor ~dest:log_path section
        with Invalid_argument _ ->
          ()
        end
      | _ -> assert false

    let _real_paths () =
      !t.Clflags.real_paths <- true
    let _absname () =
      Location.absname := true
    let _I dir =
      let dir = Misc.expand_directory Config.standard_library dir in
      !t.Clflags.include_dirs <- dir :: !t.Clflags.include_dirs
    let _init s =
      !t.Clflags.init_file <- Some s
    let _labels () =
      !t.Clflags.classic <- false
    let _no_app_funct () =
      !t.Clflags.applicative_functors <- false
    let _nolabels () =
      !t.Clflags.classic <- true
    let _nostdlib () =
      !t.Clflags.no_std_include <- true
    let _principal () =
      !t.Clflags.principal <- true
    let _rectypes () =
      !t.Clflags.recursive_types <- true
    let _strict_sequence () =
      !t.Clflags.strict_sequence <- true
    let _unsafe () =
      !t.Clflags.fast <- true
    let _version () =
      print_version ()
    let _vnum () =
      print_version_num ()
    let _w s =
      Warnings.parse_options false s
    let _warn_error s =
      Warnings.parse_options true s
    let _warn_help () =
      Warnings.help_warnings ()

    let _ignore_sigint () =
      try ignore (Sys.(signal sigint Signal_ignore))
      with Invalid_argument _ -> ()

    let anonymous s = unexpected_argument s
  end)
end
