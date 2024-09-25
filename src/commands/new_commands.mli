(* {{{ COPYING *(

     This file is part of Merlin, an helper for ocaml editors

     Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
                                Thomas Refis  <refis.thomas(_)gmail.com>
                                Simon Castellan  <simon.castellan(_)iuwt.fr>
                                Tomasz Kołodziejski  <tkolodziejski(_)gmail.com>

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

type command =
  | Command :
      string
      * Marg.docstring
      * ([ `Mandatory | `Optional | `Many ] * 'args Marg.spec) list
      * 'args
      * (Mpipeline.t -> 'args -> json)
      -> command

val all_commands : command list

(** [find_command name cmds] returns the command with name [name] in the list
    [cmds] if it exists. Raises [Not_found] if it does not. *)
val find_command : string -> command list -> command

(** [find_command name cmds] optionaly returns the command with name [name] if
    it is in the list [cmds]. *)
val find_command_opt : string -> command list -> command option
