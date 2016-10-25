let cwd = Unix.getcwd ()

let verbose = ref false

let rec split_path path acc =
  match Filename.dirname path, Filename.basename path with
  | dir, _ when dir = path -> dir :: acc
  | dir, base -> split_path dir (base :: acc)

let canonicalize_filename ?(cwd=cwd) path =
  let parts =
    match split_path path [] with
    | dot :: rest when dot = Filename.current_dir_name ->
      split_path cwd rest
    | parts -> parts
  in
  let goup path = function
    | dir when dir = Filename.parent_dir_name ->
      (match path with _ :: t -> t | [] -> [])
    | dir when dir = Filename.current_dir_name ->
      path
    | dir -> dir :: path
  in
  let parts = List.rev (List.fold_left goup [] parts) in
  let filename_concats = function
    | [] -> ""
    | root :: subs -> List.fold_left Filename.concat root subs
  in
  filename_concats parts

let split_command str =
  let comps = ref [] in
  let dirty = ref false in
  let buf   = Buffer.create 16 in
  let flush () =
    if !dirty then (
      comps := Buffer.contents buf :: !comps;
      dirty := false;
      Buffer.clear buf;
    )
  in
  let i = ref 0 and len = String.length str in
  let unescape = function
    | 'n' -> '\n'
    | 'r' -> '\r'
    | 't' -> '\t'
    |  x  -> x
  in
  while !i < len do
    let c = str.[!i] in
    incr i;
    match c with
    | ' ' | '\t' | '\n' | '\r' -> flush ()
    | '\\' ->
      dirty := true;
      if !i < len then (
        Buffer.add_char buf (unescape str.[!i]);
        incr i
      )
    | '\'' ->
      dirty := true;
      while !i < len && str.[!i] <> '\'' do
        Buffer.add_char buf str.[!i];
        incr i;
      done;
      incr i
    | '"' ->
      dirty := true;
      while !i < len && str.[!i] <> '"' do
        (match str.[!i] with
         | '\\' ->
           incr i;
           if !i < len then
             Buffer.add_char buf (unescape str.[!i]);
         | x -> Buffer.add_char buf x
        );
        incr i;
      done;
      incr i
    | x ->
      dirty := true;
      Buffer.add_char buf x
  done;
  flush ();
  List.rev !comps

let join_command =
  let add_escaped buffer str =
    Buffer.add_char buffer '\'';
    for i = 0 to String.length str - 1 do
      match str.[i] with
      | '\'' -> Buffer.add_string buffer "'\"'\"'"
      | '\x00' -> Buffer.add_string buffer "'$'\\0''"
      | c -> Buffer.add_char buffer c
    done;
    Buffer.add_char buffer '\''
  in
  function
  | [] -> ""
  | str :: strs ->
    let buffer = Buffer.create 15 in
    add_escaped buffer str;
    List.iter
      (fun str -> Buffer.add_char buffer ' '; add_escaped buffer str)
      strs;
    Buffer.contents buffer

let rec parse_args flags = function
  | "--" :: args -> args
  | (k :: args) as args0 ->
    begin match List.assoc k flags with
      | exception Not_found ->  args0
      | r ->
        begin match args with
          | [] -> failwith ("expecting argument after " ^ k)
          | v :: args ->
            r := v :: !r;
            parse_args flags args
        end
    end
  | [] -> []

type task = {
  cwd : string option;
  command : string array;
  mutable pending_inputs : int;
  inputs : entry list;
  outputs : entry list;
}

and entry = {
  name : string;
  mutable state : [`Source | `Pending | `Done of task];
  mutable dependents : task list;
}

let entries = Hashtbl.create 7

let worklist = ref []

let running_jobs = ref []

let mark_done task =
  assert (task.pending_inputs = 0);
  List.iter (fun output ->
      assert (output.state = `Pending);
      output.state <- `Done task;
      List.iter (fun task' ->
          task'.pending_inputs <- task'.pending_inputs - 1;
          assert (task'.pending_inputs >= 0);
          if task'.pending_inputs = 0 then
            worklist := task' :: !worklist
        ) output.dependents
    ) task.outputs

let filter_jobs pid =
  if !verbose then Printf.eprintf "# pid %d done\n%!" pid;
  let rec filter = function
    | (pid', task) :: rest when pid = pid' ->
      mark_done task;
      rest
    | ptask :: rest ->
      ptask :: filter rest
    | [] -> []
  in
  running_jobs := filter !running_jobs

let rec schedule_jobs () =
  match !worklist with
  | task :: worklist' when List.length !running_jobs < 4 ->
    worklist := worklist';
    let cwd' = match task.cwd with None -> cwd | Some cwd -> cwd in
    Unix.chdir cwd';
    let pid =
      Unix.create_process task.command.(0) task.command
        Unix.stdin Unix.stdout Unix.stderr
    in
    if !verbose then
      Printf.eprintf "# pid %d running task%a\n%!"
        pid
        (fun oc -> Array.iter (fun x -> output_char oc ' '; output_string oc x))
        task.command;
    Unix.chdir cwd;
    running_jobs := (pid, task) :: !running_jobs;
    schedule_jobs ()
  | _ -> ()

let rec check_jobs () =
  let pid, _ = Unix.waitpid [Unix.WNOHANG] (-1) in
  if pid = 0 then () else (
    filter_jobs pid;
    if !running_jobs <> [] then check_jobs ()
  )

let signal_child =
  let signaled = ref false in
  fun (_ : int) ->
    if !signaled then () else (
      signaled := true;
      try
        check_jobs ();
        schedule_jobs ();
        signaled := false;
      with exn ->
        signaled := false;
        raise exn
    )

let get_entry name =
  let name = canonicalize_filename name in
  try Hashtbl.find entries name
  with Not_found ->
    let state = if Sys.file_exists name then `Source else `Pending in
    let entry = { name; state; dependents = [] } in
    Hashtbl.add entries name entry;
    entry

let command_task args =
  let inputs = ref [] and outputs = ref [] and cwd = ref [] in
  match parse_args ["-i", inputs; "-o", outputs; "-cwd", cwd] args with
  | [] -> failwith "empty command"
  | command ->
    let command = Array.of_list command in
    let inputs = List.map get_entry !inputs in
    let outputs = List.map get_entry !outputs in
    List.iter (fun o -> if o.state <> `Pending then
                  failwith ("File " ^ o.name ^ " already exists")) outputs;
    let cwd = match !cwd with [] -> None | x :: _ -> Some x in
    let task = { cwd; command; inputs; outputs; pending_inputs = 0 } in
    List.iter (fun i ->
        if i.state = `Pending then (
          i.dependents <- task :: i.dependents;
          task.pending_inputs <- task.pending_inputs + 1;
        )
      ) inputs;
    if task.pending_inputs = 0 then
      worklist := task :: !worklist

let parse_command command =
  try match split_command command with
    | "task" :: args -> command_task args
    | _ -> Printf.eprintf "Unrecognized command: %S\n%!" command
  with
  | (Failure msg) -> Printf.eprintf "Failed to process %S: %s\n%!" command msg
  | exn -> Printf.eprintf "Unexpected error while processing %S: %s\n%!"
             command (Printexc.to_string exn)

let usage () =
  Printf.eprintf
    "Usage: %s <command>\n\
     Valid commands are <command>:\n\
     - server\n\
    \  Listen for commands on standard input.\n\
    \  Complete when standard input reaches EOF.\n\
     - task [-cwd ...] {-i filename} {-o filename} [--] <unix command>\n\
    \  Schedule a task consuming all \"-i\" filenames as input and producing\n\
    \  \"-o\" filenames as output.\n\
    \  <unix command> will be executed immediately if all \"-i\" filenames\n\
    \  exist on disk or when a task where they appear as \"-o\" finished\n\
    \  executing.  After execution, all tasks waiting for files in \"-o\" will\n\
    \  be rescheduled.\n"
    Sys.argv.(0)

let () =
  match Array.to_list Sys.argv with
  | _ :: "server" :: args ->
    Sys.set_signal Sys.sigchld (Sys.Signal_handle signal_child);
    verbose := List.mem "-v" args;
    let rec aux () =
      match read_line () with
      | exception End_of_file -> ()
      | "" -> aux ()
      | line ->
        if line.[0] = '#' then (
          output_substring stdout line 1 (String.length line - 1);
          output_char stdout '\n';
          flush stdout;
        ) else (
          parse_command line;
          schedule_jobs ();
        );
        aux ()
    in
    aux ();
    Sys.set_signal Sys.sigchld Sys.Signal_ignore;
    let rec wait () =
      match Unix.wait () with
      | pid, _ -> filter_jobs pid; wait ()
      | exception (Unix.Unix_error (Unix.ECHILD, _, _)) -> ()
    in
    wait ()
  | _ :: "task" :: args ->
    print_endline (join_command ("task" :: "-cwd" :: Unix.getcwd () :: args))
  | [_] ->
    usage ();
    exit 1
  | args ->
    Printf.eprintf "Unrecognized command: %s\n%!" (join_command args);
    usage ();
    exit 1
