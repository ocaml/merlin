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
    | "Caml1999I025" -> Some "4.08"
    | "Caml1999I026" -> Some "4.09"
    | "Caml1999I027" -> Some "4.10"
    | "Caml1999I028" -> Some "4.11"
    | "Caml1999I029" -> Some "4.12"
    | "Caml1999I030" -> Some "4.13"
    | "Caml1999I031" -> Some "4.14"
    | "Caml1999I032" -> Some "5.0"
    | "Caml1999I033" -> Some "5.1"
    | _ -> None

  let () = assert (to_version_opt Config.cmi_magic_number <> None)

  open Format

  let report_error ppf = function
    | Not_an_interface filename ->
        fprintf ppf "%a@ is not a compiled interface"
          Location.print_filename filename
    | Wrong_version_interface (filename, compiler_magic) ->
      let program_name = Lib_config.program_name () in
      begin match to_version_opt compiler_magic with
      | None ->
        fprintf ppf
          "Compiler version mismatch: this project seems to be compiled with a \
          version of the OCaml compiler that is not supported by this version \
          of %s. OCaml language support will not work properly until this \
          problem is fixed. \n\
          Hint: It seems that the project is built with a newer OCaml compiler \
          version that the running %s version does not know about. Make sure \
          your editor runs a version of %s that supports this version of the \
          compiler. \n\
          This diagnostic is based on the compiled interface file: %a"
          program_name program_name program_name
          Location.print_filename filename
      | Some version ->
        fprintf ppf
          "Compiler version mismatch: this project seems to be compiled with \
          version %s of the OCaml compiler, but the running %s supports OCaml \
          version %s. OCaml language support will not work properly until this \
          problem is fixed. \n\
          Hint: Make sure your editor runs a version of %s that supports the \
          correct version of the compiler. \n\
          This diagnostic is based on the compiled interface file: %a"
          version program_name
          (Option.get @@ to_version_opt Config.cmi_magic_number)
          program_name Location.print_filename filename
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
