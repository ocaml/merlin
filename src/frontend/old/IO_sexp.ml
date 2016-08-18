(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
                             Thomas Refis  <refis.thomas(_)gmail.com>
                             Simon Castellan  <simon.castellan(_)iuwt.fr>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

open Std

module Sexp = struct
  type t =
    | Cons   of t * t
    | Sym    of string
    | String of string
    | Int    of int
    | Float  of float
  let nil = Sym "nil"

  let rec sexp_of_list = function
    | [] -> nil
    | a :: tl -> Cons (a, sexp_of_list tl)

  let rec tell_sexp tell = function
    | Cons (a,b) ->
      tell "(";
      tell_sexp tell a;
      tell_cons tell b
    | Sym s    -> tell s
    | String s -> tell ("\"" ^ String.escaped s ^ "\"")
    | Int i    -> tell (string_of_int i)
    | Float f  -> tell (string_of_float f)

  and tell_cons tell = function
    | Sym "nil" -> tell ")"
    | Cons (a,b) ->
      tell " ";
      tell_sexp tell a;
      tell_cons tell b
    | sexp ->
      tell " . ";
      tell_sexp tell sexp;
      tell ")"

  let is_alpha c =
         (c >= 'a' && c <= 'z')
      || (c >= 'A' && c <= 'Z')

  let is_num c =
      (c >= '0' && c <= '9')

  let is_alphanum c = is_alpha c || is_num c

  let read_sexp getch =
    let buf = Buffer.create 10 in
    let rec read_sexp getch = function
      | ' ' | '\t' | '\n' ->
        read_sexp getch (getch ())

      | c when is_num c ->
        read_num getch c

      | '\'' | ':' | '_' as c -> read_sym getch (Some c)
      | c when is_alpha c -> read_sym getch (Some c)

      | '"' ->
        read_string getch
      | '\000' -> raise End_of_file
      | '(' ->
        let lhs, next = read_sexp getch (getch ()) in
        read_cons getch (fun rhs -> Cons (lhs, rhs)) next
      | _ -> failwith "Invalid parse"

    and read_cons getch k next =
      match (match next with Some c -> c | None -> getch ()) with
      | ' ' | '\t' | '\n' -> read_cons getch k None
      | ')' -> k nil, None
      | '.' ->
        let rhs, next = read_sexp getch (getch ()) in
        let rec aux = function
          | ')' -> k rhs
          | ' ' | '\t' | '\n' -> aux (getch ())
          | _ -> failwith "Invalid parse"
        in
        begin match next with
          | Some c -> aux c
          | None -> aux (getch ())
        end, None
      | c ->
        let cell, next = read_sexp getch c in
        read_cons getch (fun rhs -> k (Cons (cell, rhs))) next

    and read_num getch c =
      Buffer.clear buf;
      Buffer.add_char buf c;
      let is_float = ref false in
      let rec aux () =
        match getch () with
        | c when c >= '0' && c <= '9' ->
          Buffer.add_char buf c; aux ()
        | '.' | 'e' | 'E' as c ->
          is_float := true;
          Buffer.add_char buf c; aux ()
        | c ->
          let s = Buffer.contents buf in
          (if !is_float
           then Float (float_of_string s)
           else Int (int_of_string s)),
          Some c
      in
      aux ()

    and read_string getch =
      Buffer.clear buf;
      let rec aux () =
        match getch () with
        | '\000' -> failwith "Unterminated string"
        | '\\' ->
          Buffer.add_char buf '\\';
          Buffer.add_char buf (getch ());
          aux ()
        | '"' ->
          String (Scanf.unescaped (Buffer.contents buf)), None
        | c ->
          Buffer.add_char buf c;
          aux ()
      in
      aux ()

    and read_sym getch next =
      Buffer.clear buf;
      let rec aux next =
        match (match next with Some c -> c | None -> getch ()) with
        | ('\'' | '-' | ':' | '_') as c ->
          Buffer.add_char buf c;
          aux None
        | c when is_alphanum c ->
          Buffer.add_char buf c;
          aux None
        | c -> Sym (Buffer.contents buf), Some c
      in
      aux next
    in
    read_sexp getch (getch ())

  let to_buf sexp buf =
    tell_sexp (Buffer.add_string buf) sexp

  let to_string sexp =
    let buf = Buffer.create 100 in
    to_buf sexp buf;
    Buffer.contents buf

  let getch_of_substring str pos len =
    let len = pos + len in
    if pos < 0 || len > String.length str then
      invalid_arg "Sexp.getch_of_substring";
    let pos = ref pos in
    let getch () =
      if !pos < len then
        let r = str.[!pos] in
        incr pos;
        r
      else '\000'
    in
    getch

  let getch_of_string str =
    getch_of_substring str 0 (String.length str)

  let of_string str =
    fst (read_sexp (getch_of_string str))

  let of_file_descr ~on_read fd =
    let getch = ref (fun () -> '\000') in
    let rest = ref None in
    let buffer = String.create 1024 in
    let getch () =
      match !rest with
      | Some r ->
        rest := None;
        r
      | None ->
        match !getch () with
        | '\000' ->
          on_read fd;
          let read = Unix.read fd buffer 0 1024 in
          if read = 0 then '\000'
          else
            begin
              getch := getch_of_substring buffer 0 read;
              !getch ()
            end
        | c -> c
    in
    fun () ->
      try
        let sexp, rest' = read_sexp getch in
        rest := rest';
        Some sexp
      with End_of_file -> None

  let of_channel ic = of_file_descr (Unix.descr_of_in_channel ic)
end

let rec sexp_of_json =
  let open Sexp in
  let assoc_item (a,b) = Cons (Sym a, sexp_of_json b) in
  function
  | `Null       -> Sym "null"
  | `Int i      -> Int i
  | `Float f    -> Float f
  | `String s   -> String s
  | `Bool true  -> Sym "true"
  | `Bool false -> Sym "false"
  | `Assoc lst  -> Cons (Cons (Sym "assoc", Sym "nil"), sexp_of_list (List.map assoc_item lst))
  | `List lst   -> sexp_of_list (List.map sexp_of_json lst)

let rec json_of_sexp =
  let open Sexp in
  let fail msg sexp =
    failwith (msg ^ ", got: \n" ^ Sexp.to_string sexp)
  in
  let rec assoc_item = function
    | Cons (Cons (Sym a, b), c) -> (a, json_of_sexp b) :: assoc_item c
    | Sym "nil" -> []
    | sexp -> fail "expecting association (key . value)" sexp
  in
  let rec list_items = function
    | Sym "nil" -> []
    | Cons (hd, tl) -> json_of_sexp hd :: list_items tl
    | sexp -> fail "expecting list" sexp
  in
  function
  | Sym "null"  -> `Null
  | Sym "true"  -> `Bool true
  | Sym "false" -> `Bool false
  | Int i    -> `Int i
  | Float f  -> `Float f
  | String s -> `String s
  | Cons (Cons (Sym "assoc", Sym "nil"), assocs) ->
    `Assoc (assoc_item assocs)
  | Sym "nil" -> `List []
  | Cons (hd, tl) -> `List (json_of_sexp hd :: list_items tl)
  | Sym s -> `String s

let make ~on_read ~input ~output =
  (* Fix for emacs: emacs start-process doesn't distinguish between stdout and
     stderr.  So we redirect stderr to /dev/null with sexp frontend. *)
  begin match
      begin
        try Some (Unix.openfile "/dev/null" [Unix.O_WRONLY] 0o600)
        with
        | Unix.Unix_error _  ->
          if Sys.os_type = "Win32" then
            try Some (Unix.openfile "NUL" [Unix.O_WRONLY] 0o600)
            with Unix.Unix_error _ -> None
          else None
      end
      with
      | None -> ()
      | Some fd ->
        Unix.dup2 fd Unix.stderr;
        Unix.close fd
  end;
  let input' = Sexp.of_file_descr ~on_read input in
  let input' () = Option.map json_of_sexp (input' ()) in
  let buf = Buffer.create 8192 in
  let output json =
    let sexp = sexp_of_json json in
    Sexp.to_buf sexp buf;
    Buffer.add_char buf '\n';
    let contents = Buffer.contents buf in
    let rec write_contents n l =
      if l > 0 then
        let l' = Unix.write output contents n l in
        if l' > 0 then
          write_contents (n + l') (l - l')
    in
    write_contents 0 (String.length contents);
    if Buffer.length buf > 100_000
    then Buffer.reset buf
    else Buffer.clear buf
  in
  input', output
