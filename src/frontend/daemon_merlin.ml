let socket_path = lazy (
  let filename =
    match Unix.stat Sys.executable_name with
    | {Unix. st_dev; st_ino; st_mtime} ->
      Printf.sprintf "merlin_%d_%d_%d.socket"
        st_dev st_ino (int_of_float (st_mtime *. 1000.0))
    | exception exn ->
      Printf.sprintf "merlin_%s_%s.socket"
        Config.cmi_magic_number Config.cmt_magic_number
  in
  Filename.concat (Filename.get_temp_dir_name ()) filename
)

let start_daemon () =
  prerr_endline (Lazy.force socket_path)
