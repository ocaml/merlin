(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013  Frédéric Bour  <frederic.bour(_)lakaban.net>
                      Thomas Refis  <refis.thomas(_)gmail.com>
                      Simon Castellan  <simon.castellan(_)iuwt.fr>

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

(* Adjust typing environment for syntax extensions.
 * See [Fake] for AST part *)

(* Extension environment is composed of two parts:
 * - private definitions, not exposed to user, but accessed from,
 * - public definitions, those are made available to user like Pervasive
 *   module.
 * See [Typer.initial_env] for initial environment generation.
 *)

(* Private definitions are put in a fake module named "_" with the following
 * ident. Use it to test or find private definitions. *)
val ident : Ident.t

val all_extensions : unit -> string list
val set_extension : enabled:bool -> string -> unit
val enabled : unit -> string list
val disabled : unit -> string list
val register_packages : string list -> unit

(* Register extensions in environment.
 * If an extension fails to typecheck (e.g. it needs definitions from an
 * external package not loaded), it is ignored and registration
 * continues for other extensions. *)
val register : Env.t -> Env.t
