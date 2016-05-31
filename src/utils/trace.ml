open Sturgeon_stub

let destination = ref null

let set_destination cursor =
  let c = Cursor.sub cursor in
  Cursor.text cursor "\n";
  Cursor.link cursor "Clear" (fun _ -> Cursor.clear c);
  destination := c

type t = {
  cursor: Sturgeon_stub.cursor;
  limit: int;
  indent: int;
}

let start ?(limit=max_int) () =
  { cursor = Sturgeon_stub.Cursor.sub !destination; limit; indent = 0 }

let cursor t = t.cursor

let is_open t = not (Sturgeon_stub.Cursor.is_closed t.cursor)
let is_closed t = Sturgeon_stub.Cursor.is_closed t.cursor

let null = {
  cursor = null;
  limit  = 0;
  indent = 0;
}

let sub t cursor =
  {limit = t.limit - 1; cursor; indent = t.indent + 2}

let indent n = String.make n ' '

let enter_open t fmt return =
  let print str f =
    Cursor.text t.cursor (indent t.indent);
    Cursor.text t.cursor str;
    let opened = ref (t.limit <> 0) in
    let render cursor =
      Cursor.text cursor (if !opened then " [+]\n" else " [-]\n");
      let cursor = Cursor.rem_flag `Clickable cursor in
      let t' = if !opened then sub t cursor else null in
      match f t' with
      | exception exn ->
        Cursor.text cursor (indent (t.indent + 1));
        Cursor.text cursor ("RAISE " ^ Printexc.to_string exn ^ "\n");
        raise exn
      | v ->
        Cursor.text cursor (indent (t.indent + 1));
        Cursor.text cursor ("return " ^ return v ^ "\n");
        v
    in
    let c' = Cursor.clickable t.cursor
        (fun c' -> opened := not !opened; Cursor.clear c'; ignore (render c'))
    in
    render c'
  in
  Printf.ksprintf print fmt

let print_closed fmt =
  let print () f = f null in
  let open Printf in
  let open Printf_compat in
  ikfprintf print () fmt

let enter t fmt ~return =
  if is_closed t then
    print_closed fmt
  else enter_open t fmt return

let step_open t fmt return =
  let print str f =
    Cursor.text t.cursor (indent t.indent);
    Cursor.text t.cursor (str ^ " \n");
    match f {t with indent = t.indent + 2} with
    | exception exn ->
      Cursor.text t.cursor (indent (t.indent + 1));
      Cursor.text t.cursor ("RAISE " ^ Printexc.to_string exn ^ "\n");
      raise exn
    | v ->
      Cursor.text t.cursor (indent (t.indent + 1));
      Cursor.text t.cursor ("return " ^ return v ^ "\n");
      v
  in
  Printf.ksprintf print fmt

let step t fmt ~return =
  if is_closed t then
    print_closed fmt
  else step_open t fmt return
