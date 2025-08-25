(** {1 Result of typechecker}

    [Mtyper] essentially produces a typedtree, but to make sense of it
    the OCaml typechecker need to be in a specific state.

    The [result] type wraps a snapshot of this state with the typedtree to
    ensure correct accesses.
*)

type result

type typedtree =
  [ `Interface of Typedtree.signature | `Implementation of Typedtree.structure ]

type typer_cache_stats = Miss | Hit of { reused : int; typed : int }

type index_tbl =
  (Shape.Uid.t * Longident.t Location.loc, unit) Stamped_hashtable.t

val set_index_items :
  (index:index_tbl ->
  stamp:int ->
  Mconfig.t ->
  [ `Impl of Typedtree.structure_item list
  | `Intf of Typedtree.signature_item list ] ->
  unit) ->
  unit

type _ Effect.t += Partial : result -> unit Effect.t
exception Exn_after_partial

(** [run config partial parsetree]
@perform the effect Partial. It is caught in [Mpipeline.process]).

@raise [Domain_msg.Cancel] and [Domain_msg.Closing]. Botch are caught in
[Mpipeline.domain_typer]).
*)
val run :
  Mconfig.t ->
  (int * int) option ->
  _ Domain_msg.t ->
  Mreader.parsetree ->
  result

val get_env : ?pos:Msource.position -> result -> Env.t

val get_typedtree : result -> typedtree

val get_index : result -> index_tbl

val get_stamp : result -> int

val get_errors : result -> exn list

val initial_env : result -> Env.t

val get_cache_stat : result -> typer_cache_stats

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
val node_at : ?skip_recovered:bool -> result -> Lexing.position -> Mbrowse.t
