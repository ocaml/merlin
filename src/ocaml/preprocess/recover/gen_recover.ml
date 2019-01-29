(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2019  Merlin contributors

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

open MenhirSdk
open Cmly_api

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

module G = Cmly_read.Read (struct let filename = !name end)
module A = Recover_attrib.Make(G)

let () =
  let open Format in
  let ppf = Format.err_formatter in
  if !verbose then begin
    let open G in
    Lr1.iter (fun (st : lr1) ->
        fprintf ppf "\n# LR(1) state #%d\n\n" (st :> int);
        fprintf ppf "Items:\n";
        Print.itemset ppf (Lr0.items (Lr1.lr0 st));
        fprintf ppf "Transitions:\n";
        List.iter (fun (sym,(st' : lr1)) ->
            fprintf ppf " - on %a, goto #%d\n"
              Print.symbol sym
              (st' :> int)
          ) (Lr1.transitions st);
        fprintf ppf "Reductions:\n";
        List.iter (fun (t,ps) ->
            let p : production = List.hd ps in
            fprintf ppf " - on %a, reduce %d:\n  %a\n"
              Print.terminal t
              (p :> int) Print.production p
          ) (Lr1.reductions st);
      );
    Production.iter (fun (p : production) ->
        fprintf ppf "\n# Production p%d\n%a"
          (p :> int) Print.production p
      );
  end

module S = Synthesis.Make(G)(A)

let () = if !verbose then S.report Format.err_formatter

module R = Recovery.Make(G)(S)

let () = if !verbose then R.report Format.err_formatter

module E = Emitter.Make(G)(A)(S)(R)

let () = E.emit Format.std_formatter
