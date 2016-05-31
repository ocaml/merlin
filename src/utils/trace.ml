open Sturgeon_stub

let destination = ref null

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

let sub t =
  let limit = t.limit - 1 in
  if limit <= 0 then null
  else
    let indent = t.indent + 2 in
    let cursor = Cursor.sub t.cursor in
    {limit; cursor; indent}

let indent n = String.make n ' '

let enter_open t fmt result =
  let print str f =
    Cursor.text t.cursor (indent t.indent);
    Cursor.text t.cursor (str ^ " \n");
    let t' = sub t in
    match f t' with
    | exception exn ->
      Cursor.text t.cursor (indent (t.indent + 1));
      Cursor.text t.cursor ("RAISE " ^ Printexc.to_string exn ^ "\n");
      raise exn
    | v ->
      Cursor.text t.cursor (indent (t.indent + 1));
      Cursor.text t.cursor ("return " ^ result v ^ "\n");
      v
  in
  Printf.ksprintf print fmt

let print_closed fmt =
  let print () f = f null in
  let open Printf in
  let open Printf_compat in
  ikfprintf print () fmt

let enter t fmt result =
  if is_closed t then
    print_closed fmt
  else enter_open t fmt result

let step_open t fmt result =
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
      Cursor.text t.cursor ("return " ^ result v ^ "\n");
      v
  in
  Printf.ksprintf print fmt

let step t fmt result =
  if is_closed t then
    print_closed fmt
  else step_open t fmt result
