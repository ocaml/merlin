(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The grammar for lexer definitions *)

%{
  open Ocamllex_syntax

  let mk_loc loc_start loc_end =
    { Location. loc_start; loc_end; loc_ghost = false }

  let with_loc p loc_start loc_end =
    { Location. txt = p; loc = mk_loc loc_start loc_end }
%}

%token <string> Tident
%token <int> Tchar
%token <string> Tstring
%token <Location.t> Taction
%token Trule Tparse Tparse_shortest Tand Tequal Tend Tor Tunderscore Teof
       Tlbracket Trbracket Trefill
%token Tstar Tmaybe Tplus Tlparen Trparen Tcaret Tdash Tlet Tas Thash

%right Tas
%left Tor
%nonassoc CONCAT
%nonassoc Tmaybe Tstar Tplus
%left Thash
%nonassoc Tident Tchar Tstring Tunderscore Teof Tlbracket Tlparen

%start lexer_chunks
%type <Ocamllex_syntax.source_chunk list> lexer_chunks

%%

with_loc(X): x=X { with_loc x $startpos(x) $endpos(x) };

lexer_chunks:
| Taction lexer_chunks
    { Action $1 :: $2 }
| Trule definition lexer_chunks
    { Rule $2 :: $3 }
| Tand definition lexer_chunks
    { And_rule $2 :: $3 }
| Tlet name=with_loc(Tident) Tequal def=with_loc(regexp) rest=lexer_chunks
    { Let_regexp (name, def) :: rest }
| Trefill Taction lexer_chunks
    { Refill_handler $2 :: $3 }
| error lexer_chunks
    { Syntax_error (mk_loc $startpos($1) $endpos($1)) :: $2 }
| Tend
    { [] }
;

definition:
| name=with_loc(Tident) args=with_loc(Tident)* Tequal def_kind
  Tor? clauses=separated_list(Tor, clause)
    { {name; args; clauses} }
;

def_kind:
| Tparse | Tparse_shortest
  { () }
;

clause:
| pattern=with_loc(regexp) action=Taction
    { {pattern; action} }
;

regexp:
| Tunderscore
    { Characters }
| Teof
    { Eof }
| Tchar
    { Characters }
| Tstring
    { String }
| Tlbracket char_class Trbracket
    { Characters }
| regexp Tstar
    { Star $1 }
| regexp Tmaybe
    { Alternative(Epsilon, $1) }
| regexp Tplus
    { Plus $1 }
| regexp Thash regexp
    { Hash ($1, $3, mk_loc $startpos $endpos) }
| regexp Tor regexp
    { Alternative ($1, $3) }
| regexp regexp %prec CONCAT
    { Sequence ($1, $2) }
| Tlparen regexp Trparen
    { $2 }
| with_loc(Tident)
    { Named $1 }
| regexp Tas with_loc(ident)
    { Bind ($1, $3) }
;

ident:
| Tident { $1 }
;

char_class:
| Tcaret char_class1
    { () }
| char_class1
    { () }
;
char_class1:
| Tchar Tdash Tchar
    { () }
| Tchar
    { () }
| char_class1 char_class1 %prec CONCAT
    { () }
;

%%
