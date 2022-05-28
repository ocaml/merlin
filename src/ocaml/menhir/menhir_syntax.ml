(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Menhir_def

(* The type [partial_grammar] describes the abstract syntax that is produced
   by the parsers (yacc-parser and fancy-parser).

   The type [grammar] describes the abstract syntax that is obtained after one
   or more partial grammars are joined (see [PartialGrammar]). It differs in
   that declarations are organized in a more useful way and a number of
   well-formedness checks have been performed. *)

type 'a located = 'a Location.loc

(* ------------------------------------------------------------------------ *)

(* Terminals and nonterminal symbols are strings. *)

type terminal = string

type nonterminal = string

type symbol = string

(* In a somewhat fragile convention, in a partial grammar, a reference to a
   terminal symbol either is a normal identifier [LID], in which case it is
   the name of the terminal symbol, or is a quoted identifier [QID], in which
   case it is a token alias.

   Token aliases are eliminated by replacing them with the corresponding
   terminal symbols very early on during the joining of the partial grammars;
   see the module [ExpandTokenAliases].

   In a complete grammar, there are no token aliases any longer. That is,
   we keep track of the aliases that have been declared (they can be found
   via the field [tk_alias]), but we never use them, since they have been
   eliminated up front. *)

type alias = string option

(* Identifiers (which are used to refer to a symbol's semantic value) are
   strings. *)

type identifier = string

(* A file name is a string. *)

type filename = string

(* ------------------------------------------------------------------------ *)

(* A postlude is a source file fragment. *)

type postlude = Stretch.t

(* ------------------------------------------------------------------------ *)

(* OCaml semantic actions are represented as stretches. *)

type action = Stretch.t

(* ------------------------------------------------------------------------ *)

(* An attribute consists of an attribute name and an attribute payload. The
   payload is an uninterpreted stretch of source text. *)

type attribute = string located * Stretch.t

type attributes = attribute list

(* Attributes allow the user to annotate the grammar with information that is
   ignored by Menhir, but can be exploited by other tools, via the SDK. *)

(* Attributes can be attached in the following places:

   - with the grammar:         %[@bar ...]
   - with a terminal symbol:   %token FOO [@bar ...]
   - with a rule:              foo(X) [@bar ...]: ...
   - with a producer:          e = foo(quux) [@bar ...]
   - with an arbitrary symbol: %attribute FOO foo(quux) [@bar ...]

   After expanding away parameterized nonterminal symbols, things become
   a bit simpler, as %attribute declarations are desugared away. *)

(* ------------------------------------------------------------------------ *)

(* The old rule syntax. Although old, still used internally. The new syntax
   is translated down to it. *)

(* A parameter is either just a symbol or an application of a symbol to a
   nonempty tuple of parameters. Before anonymous rules have been eliminated,
   it can also be an anonymous rule, represented as a list of branches. *)

type parameter =
  | PVar of symbol located
  | PApp of symbol located * parameters
  | PAnonymous of parameterized_branch list located

and parameters = parameter list

(* ------------------------------------------------------------------------ *)

(* A producer is a pair of identifier and a parameter. In concrete syntax, it
   could be [e = expr], for instance. The identifier [e] is always present.
   (A use of the keyword [$i] in a semantic action is turned by the lexer
   and parser into a reference to an identifier [_i].) A producer carries
   a number of attributes. *)

and producer = identifier located * parameter * attributes

(* ------------------------------------------------------------------------ *)

(* A branch contains a series of producers and a semantic action. *)

and parameterized_branch = {
  pr_branch_position : Location.t;
  pr_producers       : producer list;
  pr_action          : action;
}

(* ------------------------------------------------------------------------ *)

(* A rule has a header and several branches. *)

type parameterized_rule = {
  pr_public_flag : bool;
  pr_inline_flag : bool;
  pr_nt          : nonterminal;
  pr_positions   : Location.t list;
  pr_attributes  : attributes;
  pr_parameters  : symbol list;
  pr_branches    : parameterized_branch list;
}

(* ------------------------------------------------------------------------ *)

(* The new rule syntax. *)

(* In the user's eyes, this replaces the old rule syntax, which corresponds to
   the types [parameter], [producer], [parameterized_branch], and
   [parameterized_rule] above. *)

type pattern =
  | SemPatVar of identifier located
  | SemPatWildcard
  | SemPatTilde of Location.t
  | SemPatTuple of pattern list
  (* Patterns: as in the manual. *)

type raw_action =
  (*Settings.dollars ->*) identifier option array -> action
  (* Ugly type produced by the lexer for an ACTION token. *)

type expression =
  choice_expression located
  (* A toplevel expression is a choice expression. *)

and choice_expression =
  | EChoice of seq_expression list
  (* A choice expression is a list of branches. *)

and seq_expression =
  raw_seq_expression located

and raw_seq_expression =
  | ECons of pattern * symbol_expression * seq_expression
  | ESingleton of symbol_expression
  | EAction of extended_action
  (* A sequence is either a cons [p = e1; e2]
     or a lone symbol expression [e]
     or a semantic action. *)

and symbol_expression =
  | ESymbol of symbol located * expression list * attributes
  (* A symbol expression is a symbol,
     possibly accompanied with actual parameters and attributes. *)

and extended_action =
  | XATraditional of raw_action
  | XAPointFree of Stretch.t option
  (* A semantic action is either traditional { ... } or point-free.
     There are two forms of point-free actions, <> and <id>.
     In the latter case, [id] is an OCaml identifier. *)

type rule = {
  r_lhs: symbol located;
  r_attributes: attributes;
  r_formals: symbol located list;
  r_rhs: expression;
}

(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* A declaration. (Only before joining.) *)

type declaration =
  (* Raw OCaml code. *)
  | DCode of Stretch.t
  (* Raw OCaml functor parameter. *)
  | DParameter of Stretch.t (* really a stretch *)
  (* Terminal symbol (token) declaration. *)
  | DToken of Stretch.t option * terminal * alias * attributes
  (* Start symbol declaration. *)
  | DStart of nonterminal
  (* Priority and associativity declaration. *)
  | DTokenProperties of terminal
  (* Type declaration. *)
  | DType of Stretch.t * parameter
  (* Grammar-level attribute declaration. *)
  | DGrammarAttribute of attribute
  (* Attributes shared among multiple symbols, i.e., [%attribute]. *)
  | DSymbolAttributes of parameter list * attributes
  (* On-error-reduce declaration. *)
  | DOnErrorReduce of parameter

(* ------------------------------------------------------------------------ *)

(* A partial grammar. (Only before joining.) *)

type partial_grammar = {
  pg_filename     : filename;
  pg_postlude     : postlude option;
  pg_declarations : declaration located list;
  pg_rules        : parameterized_rule list;
}

(* ------------------------------------------------------------------------ *)
