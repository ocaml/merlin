(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2024  Frédéric Bour  <frederic.bour(_)lakaban.net>
                             Thomas Refis  <refis.thomas(_)gmail.com>
                             Simon Castellan  <simon.castellan(_)iuwt.fr>
                             Arthur Wendling <arthur(_)tarides.com>
                             Xavier Van de Woestyne <xaviervdw(_)gmail.com>

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

(** Search by type in the current environment. *)

(** A Lazy trie of the potentials values. *)
type t

(** Initiailzie the trie with a given list of directories. *)
val make_trie : Env.t -> string list -> t

(** Compute the list of candidates from a query inside a given environment. *)
val run :
  ?limit:int ->
  Mconfig.t ->
  Mtyper.typedtree ->
  (string * Location.t) list ->
  Lexing.position ->
  Env.t ->
  Merlin_sherlodoc.Query_parser.t
  -> t
  -> (int * string * Types.value_description * string option) list
