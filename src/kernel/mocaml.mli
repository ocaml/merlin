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

(* Build settings *)
val setup_config : Mconfig.t -> unit

(* Instance of environment cache & btype unification log  *)
type typer_state

val new_state : unit_name:string -> typer_state
val with_state : typer_state -> (unit -> 'a) -> 'a
val is_current_state : typer_state -> bool

(* Replace Outcome printer *)
val default_printer :
  Format.formatter -> Extend_protocol.Reader.outcometree -> unit

val with_printer :
  (Format.formatter -> Extend_protocol.Reader.outcometree -> unit) ->
  (unit -> 'a) -> 'a

(* Clear caches, remove all items *)
val clear_caches : unit -> unit

(* Flush caches, remove outdated items *)
val flush_caches : ?older_than:float -> unit -> unit
