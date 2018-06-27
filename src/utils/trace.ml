(*open Sturgeon_stub*)

let ikfprintf =
  let open CamlinternalFormatBasics in
  let open CamlinternalFormat in
  fun k oc (Format (fmt, _)) ->
    make_printf (fun oc _ -> k oc) oc End_of_acc fmt

(*let destination = ref null*)

(*let set_destination cursor =
  let c = Cursor.sub cursor in
  Cursor.text cursor "\n";
  Cursor.link cursor "Clear" (fun _ -> Cursor.clear c);
  destination := c*)

(*let start ?(limit=max_int) () =
  { cursor = Sturgeon_stub.Cursor.sub !destination; limit; indent = 0 }*)

(*let cursor t = t.cursor*)

(*let is_open t = not (Sturgeon_stub.Cursor.is_closed t.cursor)*)
(*let is_closed t = Sturgeon_stub.Cursor.is_closed t.cursor*)

type t = {
  (*cursor: Sturgeon_stub.cursor;*)
  limit: int;
  indent: int;
}

let start ?(limit=max_int) () =
  { limit; indent = 0 }

let is_open t = t.limit > 0
let is_closed t = not (is_open t)

let null = {
  (*cursor = null;*)
  limit  = 0;
  indent = 0;
}

(*
let sub t =
  {limit = t.limit - 1; indent = t.indent + 2}
*)

let indent n = String.make n ' '

(*let trace_buffer, trace_formatter =
  let buffer = Buffer.create 4096 in
  buffer, Format.formatter_of_buffer buffer

let reset_trace_formatter () =
  Format.pp_flush_formatter trace_formatter;
  Buffer.reset trace_buffer

let flush_trace_formatter () =
  Format.pp_flush_formatter trace_formatter;
  let result = Buffer.contents trace_buffer in
  Buffer.reset trace_buffer;
  result*)

let format_return offset return f x =
  match f x with
  | exception exn ->
    prerr_endline (indent offset ^ "RAISE " ^ Printexc.to_string exn);
    raise exn
  | v ->
    let msg =
      try return () v
      with _ -> assert false
    in
    prerr_endline (indent offset ^ "return: " ^ msg);
    v

let enter_open t fmt return =
  let print str f =
    prerr_endline (indent t.indent ^ str);
    format_return (t.indent + 1) return f
      {t with indent = t.indent + 2}
  in
  Printf.ksprintf print fmt

let print_closed fmt =
  let print () f = f null in
  ikfprintf print () fmt

let enter t fmt ~return =
  if is_closed t then
    print_closed fmt
  else enter_open t fmt return

let message t fmt =
  if is_closed t then
    ikfprintf ignore () fmt
  else
    let print str = prerr_endline (indent t.indent ^ str) in
    Printf.ksprintf print fmt

(*let step_open t fmt return =
  let print str f =
    prerr_endline (indent t.indent ^ str);
    format_return (t.indent + 1) return f
      {t with indent = t.indent + 2}
  in
  Printf.ksprintf print fmt

let step t fmt ~return =
  if is_closed t then
    print_closed fmt
  else step_open t fmt return*)
