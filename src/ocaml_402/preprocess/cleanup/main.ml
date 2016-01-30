open MenhirSdk
open Cmly_format

let name = ref ""
let verbose = ref false

let usage () =
  Printf.eprintf "Usage: %s [-v] file.cmly\n"
    Sys.argv.(0);
  exit 1

let () =
  for i = 1 to Array.length Sys.argv - 1 do
    if Sys.argv.(i) = "-v" then
      verbose := true
    else if !name = "" then
      name := Sys.argv.(i)
    else
      usage ()
  done;
  if !name = "" then
    usage ()

module S = Synthesis.Make(struct
    let grammar = Cmly_io.read_file !name
    let cost_of_prod _ = 0.0
    let penalty_of_item _ = 0.0
    let cost_of_symbol = function
      | T _ -> 1.0
      | N _ -> infinity
  end)

let () = if !verbose then S.report Format.err_formatter

module R = Recovery.Make(S)

let () = if !verbose then R.report Format.err_formatter

module E = Emitter.Make(S)

let () = E.emit R.recover Format.std_formatter
