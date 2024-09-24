module Level = struct
  type t = Debug | Warning | Error

  let int_of_t = function
    | Debug -> 0
    | Warning -> 1
    | Error -> 2

  let string_of_t = function
    | Debug -> "debug"
    | Warning -> "warning"
    | Error -> "error"

  let ( >= ) a b = int_of_t a >= int_of_t b
end

let log_level = ref Level.Error
let set_log_level level = log_level := level

let log ~level =
  let print =
    let formatter =
      if level = Level.Error then Format.err_formatter else Format.std_formatter
    in
    Format.fprintf formatter "[%s] %s\n%!" (Level.string_of_t level)
  in
  if Level.(level >= !log_level) then Format.kasprintf print
  else Format.ikfprintf ignore Format.std_formatter

let debug fmt = log ~level:Level.Debug fmt
let warn fmt = log ~level:Level.Warning fmt
let error fmt = log ~level:Level.Error fmt
