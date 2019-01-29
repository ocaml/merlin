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

(** {0 Argument parsing library which fold over arguments}

    Specifications of arguments is split in two passes:
    - [_ table] for parsing global arguments (compiler flags, merlin
      configuration)
    - a (string * _ t) for parsing command local arguments
*)

(** Action associated to a flag updating a state of type 'acc.
    It takes a list of arguments and either succeeds returning untouched
    arguments or fails raising an exception. *)
type 'acc t = string list -> 'acc -> string list * 'acc

(** A table mapping a flag to the corresponding action *)
type 'acc table = (string, 'acc t) Hashtbl.t

(** {1 Combinators for building actions} *)

(** Action updating state and not consuming any argument *)
val unit : ('acc -> 'acc) -> 'acc t

(** Action consuming a single argument *)
val param : string -> (string -> 'acc -> 'acc) -> 'acc t

(** Action consuming a boolean argument *)
val bool : (bool -> 'acc -> 'acc) -> 'acc t

(** Action doing nothing *)
val unit_ignore : 'acc t

(** Action doing nothing and dropping one argument *)
val param_ignore : 'acc t

(** {1 Parsing of argument lists} *)

type docstring = string

type 'a spec = (string * docstring * 'a t)

(** Consume at most one flag from the list, returning updated state or
    [None] in case of failure.
    Warning function is called with an error message in case of incorrect
    use.  *)
val parse_one :
  warning:(string -> unit) ->
  'global table -> 'local spec list ->
  string list -> 'global -> 'local ->
  (string list * 'global * 'local) option

(** Consume all arguments from the input list, calling warning for incorrect
    ones and resuming parsing after. *)
val parse_all :
  warning:(string -> unit) ->
  'global table -> 'local spec list ->
  string list -> 'global -> 'local ->
  'global * 'local
