open Std

type command =
    Command : string * Marg.docstring * 'args Marg.spec list * 'args *
              (Trace.t * Mconfig.t * Msource.t -> 'args -> json) -> command

val all_commands : command list

val find_command : string -> command list -> command
