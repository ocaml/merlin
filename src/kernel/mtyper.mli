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

(** {1 Result of typechecker}

    [Mtyper] essentially produces a typedtree, but to make sense of it
    the OCaml typechecker need to be in a specific state.

    The [result] type wraps a snapshot of this state with the typedtree to
    ensure correct accesses.
*)

type result

type typedtree = [
  | `Interface of Typedtree.signature
  | `Implementation of Typedtree.structure
]

val run : Mconfig.t -> Mreader.parsetree -> result

val with_typer : result -> (unit -> 'a) -> 'a

val get_env : ?pos:Msource.position -> result -> Env.t

val get_typedtree : result -> typedtree

val get_errors : result -> exn list

(** Heuristic to find suitable environment to complete / type at given position.
 *  1. Try to find environment near given cursor.
 *  2. Check if there is an invalid construct between found env and cursor :
 *    Case a.
 *      > let x = valid_expr ||
 *      The env found is the right most env from valid_expr, it's a correct
 *      answer.
 *    Case b.
 *      > let x = valid_expr
 *      > let y = invalid_construction||
 *      In this case, the env found is the same as in case a, however it is
 *      preferable to use env from enclosing module rather than an env from
 *      inside x definition.
 *)
val node_at :
  ?skip_recovered:bool -> result -> Lexing.position -> Mbrowse.t
