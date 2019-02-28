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

open Std

module Cmi = struct
  type error =
    | Not_an_interface of string
    | Wrong_version_interface of string * string
    | Corrupted_interface of string

  exception Error of error

  let to_version_opt = function
    | "Caml1999I017" -> Some "4.02"
    | "Caml1999I020" -> Some "4.03"
    | "Caml1999I021" -> Some "4.04 or 4.05"
    | "Caml1999I022" -> Some "4.06"
    | "Caml1999I023" -> Some "4.07.0"
    | "Caml1999I024" -> Some "4.07.1"
    | _ -> None

  open Format

  let report_error ppf = function
    | Not_an_interface filename ->
        fprintf ppf "%a@ is not a compiled interface"
          Location.print_filename filename
    | Wrong_version_interface (filename, compiler_magic) ->
      begin match to_version_opt compiler_magic with
      | None ->
        fprintf ppf
          "%a@ seems to be compiled with a version of OCaml that is not@.\
           supported by Merlin."
          Location.print_filename filename
      | Some version ->
        fprintf ppf
          "%a@ seems to be compiled with OCaml %s.@.\
           But this instance of Merlin handles OCaml %s."
          Location.print_filename filename version
          (Option.get @@ to_version_opt Config.cmi_magic_number)
      end
    | Corrupted_interface filename ->
        fprintf ppf "Corrupted compiled interface@ %a"
          Location.print_filename filename

  let () =
    Location.register_error_of_exn
      (function
        | Error err -> Some (Location.error_of_printer_file report_error err)
        | _ -> None
      )
end
