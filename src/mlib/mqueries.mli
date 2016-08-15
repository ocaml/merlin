open Std

type input = {
  source    : Msource.t;
  reader    : Mreader.t;
  config    : Mconfig.t;
  parsetree : Mreader.parsetree;
}

val dump_input : input -> json

type command =
    Command : string * Marg.docstring * 'args Marg.spec list * 'args *
              (input -> 'args -> json) -> command

val queries : command list

val find_command : string -> command list -> command
