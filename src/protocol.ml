(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013  Frédéric Bour  <frederic.bour(_)lakaban.net>
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

type io = Json.json Stream.t * (Json.json -> unit)
type io_maker = input:in_channel -> output:out_channel -> io

let section = Logger.(`protocol)

exception Protocol_failure of string

let json_log  (input,output) =
  let log_input json = Logger.log section ~prefix:"<" (Json.to_string json); json in
  let log_output json = Logger.log section ~prefix:">" (Json.to_string json); json in
  let input' =
    Stream.from
    begin fun _ ->
      try Some (log_input (Stream.next input))
      with Stream.Failure -> None
    end
  in
  let output' json = output (log_output json) in
  input', output'

let json_make ~input ~output =
  let input  = Json.stream_from_channel input in
  let output' = Json.to_channel output in
  let output json =
    output' json;
    output_char output '\n';
    flush output
  in
  input, output

let makers = ref ["json", ("(default) simple JSON-based protocol", json_make)]

let register_protocol ~name ~desc inst =
  makers := (name, (desc,inst)) :: !makers

let make' = ref json_make
let make ~input ~output = 
  let io = !make' ~input ~output in
  if Logger.is_monitored section then json_log io else io

let select_frontend name =
  try make' := snd (List.assoc name !makers)
  with Not_found ->
    if name <> "help" then
      prerr_endline 
        ("Unknown protocol '" ^ name ^ "' (maybe check build configuration)\n"); 
    prerr_endline "Choose protocol to use for communication. Known protocols:";
    List.iter (fun (name, (desc, _)) -> 
        prerr_endline (name ^ "\t" ^ desc))
      !makers;
    exit 1

let return l = `List [`String "return" ; l]

let error_catcher = ref (fun _ -> None)
let fail = function
  | Protocol_failure s ->
    prerr_endline ("Fatal protocol failure. " ^ s);
    exit (-1)
  | Failure s -> `List [`String "failure"; `String s]
  | exn -> match !error_catcher exn with
      | Some (_,error) -> `List [`String "error"; error]
      | None -> `List [`String "exception"; `String (Printexc.to_string exn)]
let protocol_failure s = raise (Protocol_failure s)

let make_pos (pos_lnum, pos_cnum) =
  Lexing.({ pos_fname = "" ; pos_lnum ; pos_cnum ; pos_bol = 0 })

let pos_to_json pos =
  Lexing.(`Assoc ["line", `Int pos.pos_lnum;
                  "col", `Int (pos.pos_cnum - pos.pos_bol)])
                  (*"offset", `Int pos.pos_cnum])*)

let pos_of_json = function
  | `Assoc props ->
    begin try match List.assoc "line" props, List.assoc "col" props with
      | `Int line, `Int col -> make_pos (line,col)
      | _ -> failwith "Incorrect position"
    with Not_found -> failwith "Incorrect position"
    end
  | _ -> failwith "Incorrect position"

let with_location loc assoc =
  `Assoc (("start", pos_to_json loc.Location.loc_start) ::
          ("end",   pos_to_json loc.Location.loc_end) ::
          assoc)
