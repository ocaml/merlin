type client = {
  stdin  : Unix.file_descr;
  stdout : Unix.file_descr;
  stderr : Unix.file_descr;
  argv   : string array;
}

external connect : Unix.file_descr -> client option = "ml_merlin_daemon_connect"

let get_fd arg =
  try
    let fd : Unix.file_descr = Obj.magic (int_of_string arg) in
    ignore (Unix.fstat fd);
    fd
  with _ ->
    failwith ("invalid server socket (" ^ arg ^ ")")
