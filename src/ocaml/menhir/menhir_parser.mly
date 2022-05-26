(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* This is the fancy version of the parser, to be processed by menhir.
   It is kept in sync with [Parser], but exercises menhir's features. *)

(* As of 2014/12/02, the $previouserror keyword and the --error-recovery
   mode no longer exist. Thus, we replace all calls to [Error.signal]
   with calls to [Error.error], and report just one error. *)

(* ------------------------------------------------------------------------- *)
(* Imports. *)

%{

open Menhir_def
open Menhir_syntax
open Positions

(* An injection of symbol expressions into choice expressions. *)

let inject (e : symbol_expression located) : expression =
  Positions.pmap (fun pos e ->
    let branch =
      Branch (
          Positions.with_pos pos (ESingleton e)
      )
    in
    EChoice [ branch ]
  ) e

(* When a stretch has been created by [Lexer.mk_stretch] with [parenthesize]
   set to [true], it includes parentheses. In some (rare) cases, this is
   undesirable. The following function removes the parentheses a posteriori.
   They are replaced with whitespace, so as to not alter column numbers. *)

(*let rec find s n i =
  assert (i < n);
  if s.[i] = '(' then i
  else begin
    assert (s.[i] = ' ');
    find s n (i+1)
  end*)

(*

let unparenthesize (s : string) : string =
  let n = String.length s in
  (* The string [s] must end with a closing parenthesis. *)
  assert (n >= 2 && s.[n-1] = ')');
  (* The string [s] must begin with a certain amount of spaces
     followed with an opening parenthesis. Find its offset [i]. *)
  let i = find s n 0 in
  (* Create a copy without the parentheses. *)
  let b = Bytes.of_string s in
  Bytes.set b i ' ';
  Bytes.set b (n-1) ' ';
  Bytes.to_string b

let unparenthesize (s : Stretch.t) : Stretch.t =
  { s with stretch_content = unparenthesize s.stretch_content }

let unparenthesize (o : Stretch.t option) : Stretch.t option =
  Option.map unparenthesize o

*)

%}

(* ------------------------------------------------------------------------- *)
(* Tokens. *)

%token
  TOKEN            "%token"
  TYPE             "%type"
  LEFT             "%left"
  RIGHT            "%right"
  NONASSOC         "%nonassoc"
  START            "%start"
  PREC             "%prec"
  PUBLIC           "%public"
  COLON            ":"
  BAR              "|"
  EOF              ""
  EQUAL            "="
  INLINE           "%inline"
  LPAREN           "("
  RPAREN           ")"
  COMMA            ","
  QUESTION         "?"
  STAR             "*"
  PLUS             "+"
  PARAMETER        "%parameter"
  ON_ERROR_REDUCE  "%on_error_reduce"
  PERCENTATTRIBUTE "%attribute"
  SEMI             ";"

%token <string Menhir_def.Positions.located>
  LID              "lident"
  UID              "UIdent"
  QID              "\"alias\""

%token <Menhir_def.Stretch.t>
  HEADER           "%{ header %}"

%token <Menhir_def.Stretch.ocamltype>
  OCAMLTYPE        "<unit>"

%token <Menhir_def.Stretch.t Lazy.t>
  PERCENTPERCENT   "%%"

%token <Menhir_syntax.raw_action>
  ACTION           "{}"

%token <Menhir_syntax.attribute>
  ATTRIBUTE        "[@foo]"
  GRAMMARATTRIBUTE "%[@foo]"

(* For the new rule syntax: *)
%token
  LET              "let"
  TILDE            "~"
  UNDERSCORE       "_"
  COLONEQUAL       ":="
  EQUALEQUAL       "=="

(* ------------------------------------------------------------------------- *)
(* Type annotations and start symbol. *)

(*%type <ParserAux.early_producer> producer*)
(*%type <ParserAux.early_production> production*)
%start <Menhir_syntax.partial_grammar> grammar

(* ------------------------------------------------------------------------- *)
(* Priorities. *)

(* These declarations solve a shift-reduce conflict in favor of shifting: when
   the right-hand side of an old-style rule begins with a leading bar, this
   bar is understood as an (insignificant) leading optional bar, *not* as an
   empty right-hand side followed by a bar. This ambiguity arises due to the
   possibility for several productions to share a single semantic action.
   The new rule syntax does not have this possibility, and has no ambiguity. *)

%nonassoc no_optional_bar
%nonassoc BAR

(* ------------------------------------------------------------------------- *)
(* On-error-reduce declarations. *)

(* These declarations reduce the number of states where an error can occur,
   thus reduce the number of syntax error messages that we have to write in
   parserMessages.messages. *)

%on_error_reduce old_rule
%on_error_reduce list(ATTRIBUTE)
%on_error_reduce action_expression
%on_error_reduce separated_nonempty_list(COMMA,symbol)
%on_error_reduce separated_nonempty_list(COMMA,pattern)
%on_error_reduce loption(delimited(LPAREN,separated_nonempty_list(COMMA,lax_actual),RPAREN))
%on_error_reduce loption(delimited(LPAREN,separated_nonempty_list(COMMA,expression),RPAREN))

%%

(* ------------------------------------------------------------------------- *)
(* A grammar consists of declarations and rules, followed by an optional
   postlude, which we do not parse. *)

grammar:
  ds = flatten(declaration*)
  PERCENTPERCENT
  rs = rule*
  t = postlude
    {
      {
        pg_filename          = ""; (* filled in by the caller *)
        pg_declarations      = ds;
        pg_rules             = rs;
        pg_postlude          = t
      }
    }

(* ------------------------------------------------------------------------- *)
(* A declaration is an %{ OCaml header %}, or a %token, %start,
   %type, %left, %right, or %nonassoc declaration. *)

declaration:

| h = HEADER (* lexically delimited by %{ ... %} *)
    { [ with_loc $loc (DCode h) ] }

| TOKEN ty = OCAMLTYPE? ts = clist(terminal_alias_attrs)
    { List.map (Positions.map (fun (terminal, alias, attrs) ->
        DToken (ty, terminal, alias, attrs)
      )) ts }

| START t = OCAMLTYPE? nts = clist(nonterminal)
    (* %start <ocamltype> foo is syntactic sugar for %start foo %type <ocamltype> foo *)
    {
      match t with
      | None ->
          List.map (Positions.map (fun nonterminal -> DStart nonterminal)) nts
      | Some _t ->
          failwith "TODO"
          (* Misc.mapd (fun ntloc ->
            Positions.mapd (fun nt -> DStart nt, DType (t, ParameterVar ntloc)) ntloc) nts *)
    }

| TYPE _t = OCAMLTYPE _ss = clist(strict_actual)
    { failwith "TODO" }
    (* List.map (Positions.map (fun nt -> DType (t, nt)))
         (List.map Parameters.with_pos ss) *)

| _k = priority_keyword _ss = clist(symbol)
    { failwith "TODO" }
    (* let prec = ParserAux.new_precedence_level $loc(k) in
       List.map (Positions.map (fun symbol -> DTokenProperties (symbol, k, prec))) ss *)

| PARAMETER t = OCAMLTYPE
    { [ with_loc $loc (DParameter t) ] }

| attr = GRAMMARATTRIBUTE
    { [ with_loc $loc (DGrammarAttribute attr) ] }

| PERCENTATTRIBUTE actuals = clist(strict_actual) attrs = ATTRIBUTE+
    { [ with_loc $loc (DSymbolAttributes (actuals, attrs)) ] }

| ON_ERROR_REDUCE _ss = clist(strict_actual)
    { failwith "TODO" }
    (* let prec = ParserAux.new_on_error_reduce_level() in
       List.map (Positions.map (fun nt -> DOnErrorReduce (nt, prec)))
         (List.map Parameters.with_pos ss) *)

| SEMI
    { [] }

(* This production recognizes tokens that are valid in the rules section,
   but not in the declarations section. This is a hint that a %% was
   forgotten. *)

| rule_specific_token
    {
      Error.error [Positions.import $loc]
        "syntax error inside a declaration.\n\
         Did you perhaps forget the %%%% that separates declarations and rules?"
    }

priority_keyword:
  LEFT
    { LeftAssoc }
| RIGHT
    { RightAssoc }
| NONASSOC
    { NonAssoc }

%inline rule_specific_token:
| PUBLIC
| INLINE
| COLON
| LET
| EOF
    { () }

(* ------------------------------------------------------------------------- *)
(* Our lists of symbols are separated with optional commas. Order is
   irrelevant. *)

%inline clist(X):
  xs = separated_nonempty_list(COMMA?, X)
    { xs }

(* ------------------------------------------------------------------------- *)
(* A symbol is a terminal or nonterminal symbol. *)

(* One would like to require nonterminal symbols to begin with a lowercase
   letter, so as to lexically distinguish them from terminal symbols, which
   must begin with an uppercase letter. However, for compatibility with
   ocamlyacc, this is impossible. It can be required only for nonterminal
   symbols that are also start symbols. *)

(* We also accept token aliases in place of ordinary terminal symbols.
   Token aliases are quoted strings. *)

symbol:
  id = LID
| id = UID
| id = QID
    { id }

(* ------------------------------------------------------------------------- *)
(* Terminals must begin with an uppercase letter. Nonterminals that are
   declared to be start symbols must begin with a lowercase letter. *)

(* In declarations, terminals must be UIDs, but we may also declare
   token aliases, which are QIDs. *)

%inline terminal_alias_attrs:
  id = UID alias = QID? attrs = ATTRIBUTE*
    { let alias = Option.map Positions.value alias in
      Positions.map (fun uid -> uid, alias, attrs) id }

%inline nonterminal:
  id = LID
    { id }

(* ------------------------------------------------------------------------- *)
(* A rule is expressed either in the traditional (yacc-style) syntax or in
   the new syntax. *)

%inline rule:
  old_rule
    { $1 }
| new_rule
    (* The new syntax is converted on the fly to the old syntax. *)
    { failwith "TODO" (*NewRuleSyntax.rule $1*) }

(* ------------------------------------------------------------------------- *)
(* A rule defines a symbol. It is optionally declared %public, and optionally
   carries a number of formal parameters. The right-hand side of the definition
   consists of a list of productions. *)

old_rule:
  flags = flags            (* flags *)
  symbol = symbol          (* the symbol that is being defined *)
  attributes = ATTRIBUTE*
  params = plist(symbol)   (* formal parameters *)
  COLON
  optional_bar
  branches = branches
  SEMI*
    {
      let public, inline = flags in
      let rule = {
        pr_public_flag = public;
        pr_inline_flag = inline;
        pr_nt          = Positions.value symbol;
        pr_positions   = [ Positions.position symbol ];
        pr_attributes  = attributes;
        pr_parameters  = List.map Positions.value params;
        pr_branches    = branches
      }
      in rule
    }

%inline branches:
  prods = separated_nonempty_list(BAR, production_group)
    { List.flatten prods }

flags:
  (* epsilon *)
    { false, false }
| PUBLIC
    { true, false }
| INLINE
    { false, true }
| PUBLIC INLINE
| INLINE PUBLIC
    { true, true }

optional_bar:
  (* epsilon *) %prec no_optional_bar
| BAR
    { () }

(* ------------------------------------------------------------------------- *)
(* A production group consists of a list of productions, followed by a
   semantic action and an optional precedence specification. *)

production_group:
  _productions = separated_nonempty_list(BAR, production)
  _action = ACTION
  _oprec2 = ioption(precedence)
  { failwith "TODO" }
      (*
        (* If multiple productions share a single semantic action, check
           that all of them bind the same names. *)
        ParserAux.check_production_group productions;
        (* Then, *)
        List.map (fun (producers, oprec1, level, pos) ->
          (* Replace [$i] with [_i]. *)
          let pr_producers = ParserAux.normalize_producers producers in
          (* Distribute the semantic action. Also, check that every [$i]
             is within bounds. *)
          let names = ParserAux.producer_names producers in
          let pr_action = action Settings.dollars names in
          {
            pr_producers;
            pr_action;
            pr_branch_prec_annotation   = ParserAux.override pos oprec1 oprec2;
            pr_branch_production_level  = level;
            pr_branch_position          = pos
          })
        productions
      *)

precedence:
  PREC symbol = symbol
    { symbol }

(* ------------------------------------------------------------------------- *)
(* A production is a list of producers, optionally followed by a
   precedence declaration. *)

production:
  producers = producer* oprec = ioption(precedence)
    { producers,
      oprec,
      (*ParserAux.new_production_level(),*)
      Positions.import $loc
    }

(* ------------------------------------------------------------------------- *)
(* A producer is an actual parameter, possibly preceded by a
   binding, and possibly followed with attributes.

   Because both [ioption] and [terminated] are defined as inlined by
   the standard library, this definition expands to two productions,
   one of which begins with id = LID, the other of which begins with
   p = actual. The token LID is in FIRST(actual),
   but the LR(1) formalism can deal with that. If [option] was used
   instead of [ioption], an LR(1) conflict would arise -- looking
   ahead at LID would not allow determining whether to reduce an
   empty [option] or to shift. *)

producer:
| id = ioption(terminated(LID, EQUAL)) p = actual attrs = ATTRIBUTE* SEMI*
    { position (with_loc $loc ()), id, p, attrs }

(* ------------------------------------------------------------------------- *)
(* The ideal syntax of actual parameters includes:
   1. a symbol, optionally applied to a list of actual parameters;
   2. an actual parameter followed with a modifier;
   3. an anonymous rule. (Not delimited by parentheses! Otherwise
      one would often end up writing two pairs of parentheses.) *)

(* In order to avoid a few ambiguities, we restrict this ideal syntax as
   follows:
   a. Within a %type declaration, we use [strict_actual], which
      allows 1- and 2- (this is undocumented; the documentation says we
      require a symbol) but not 3-, which would not make semantic sense
      anyway.
   b. Within a producer, we use [actual], which allows 1- and
      2- but not 3-. Case 3- is allowed by switching to [lax_actual]
      within the actual arguments of an application, which are clearly
      delimited by parentheses and commas.
   c. In front of a modifier, we can never allow [lax_actual],
      as this would create an ambiguity: basically, [A | B?] could be
      interpreted either as [(A | B)?] or as [A | (B?)].
*)

%inline generic_actual(A, B):
(* 1- *)
  _symbol = symbol _actuals = plist(A)
    { failwith "TODO" (*Parameters.app symbol actuals*) }
(* 2- *)
| p = B m = located(modifier)
    { ParameterApp (m, [ p ]) }

strict_actual:
  p = generic_actual(strict_actual, strict_actual)
    { p }

actual:
  p = generic_actual(lax_actual, actual)
    { p }

lax_actual:
  p = generic_actual(lax_actual, (* cannot be lax_ *) actual)
    { p }
(* 3- *)
| (* leading bar disallowed *)
  branches = located(branches)
    { ParameterAnonymous branches }
    (* 2016/05/18: we used to eliminate anonymous rules on the fly during
       parsing. However, when an anonymous rule appears in a parameterized
       definition, the fresh nonterminal symbol that is created should be
       parameterized. This was not done, and is not easy to do on the fly,
       as it requires inherited attributes (or a way of simulating them).
       We now use explicit abstract syntax for anonymous rules. *)

(* ------------------------------------------------------------------------- *)
(* The "?", "+", and "*" modifiers are short-hands for applications of
   certain parameterized nonterminals, defined in the standard library. *)

modifier:
  QUESTION
    { "option" }
| PLUS
    { "nonempty_list" }
| STAR
    { "list" }

(* ------------------------------------------------------------------------- *)
(* A postlude is announced by %%, but is optional. *)

postlude:
  EOF
    { None }
| p = PERCENTPERCENT (* followed by actual postlude *)
    { Some (Lazy.force p) }

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* The new rule syntax. *)

(* Whereas the old rule syntax allows a nonterminal symbol to begin with an
   uppercase letter, the new rule syntax disallows it. The left-hand side of a
   new rule must be a lowercase identifier [LID]. *)

(* A new rule *cannot* be terminated by a semicolon. (This is contrast with a
   traditional rule, which can be followed with any number of semicolons.) We
   are forced to forbid the use of semicolons are as a rule terminator because
   they are used already as a sequencing construct. Permitting both uses would
   give rise to a shift/reduce conflict that we would not be able to solve. *)

new_rule:
| rule_public     = boption(PUBLIC)
  LET
  rule_lhs        = LID
  rule_attributes = ATTRIBUTE*
  rule_formals    = plist(symbol)
  rule_inline     = equality_symbol
  rule_rhs        = expression
    {{
       rule_public;
       rule_inline;
       rule_lhs;
       rule_attributes;
       rule_formals;
       rule_rhs;
    }}

(* A new rule is written [let foo := ...] or [let foo == ...].
   In the former case, we get an ordinary nonterminal symbol;
   in the latter case, we get an %inline nonterminal symbol. *)

equality_symbol:
  COLONEQUAL
    { false }
| EQUALEQUAL
    { true  }

(* The right-hand side of a new rule is an expression. *)

(* An expression is a choice expression. *)

expression:
  e = located(choice_expression)
    { e }

(* A choice expression is a bar-separated list of alternatives, with an
   optional leading bar, which is ignored. Each alternative is a sequence
   expression. *)

(* We cannot allow a choice expression to be empty, even though that would
   make semantic sense (the empty sum is void). Indeed, that would create a
   shift/reduce conflict: after reading [def x = y], it would be unclear
   whether this is a definition of [x] as an alias for [y], or a definition of
   [x] as an alias for the empty sum, followed with an old-style rule that
   happens to begin with [y]. *)

%inline choice_expression:
  branches = preceded_or_separated_nonempty_llist(BAR, branch)
    { EChoice branches }

%inline branch:
  e = seq_expression
    { Branch e (* ParserAux.new_production_level() *) }

(* A sequence expression takes one of the following forms:

         e1; e2     a sequence that binds no variables (sugar for _ = e1; e2)
     p = e1; e2     a sequence that binds the variables in the pattern p

   or is an symbol expression or an action expression. *)

(* Allowing an symbol expression [e] where a sequence expression is expected
   can be understood as syntactic sugar for [x = e; { x }]. *)

(* In a sequence [e1; e2] or [p = e1; e2], the left-hand expression [e1] is
   *not* allowed to be an action expression. That would be a Bison-style
   midrule action. Instead, one must explicitly write [midrule({ ... })]. *)

(* In a sequence, the semicolon cannot be omitted. This is in contrast with
   old-style rules, where semicolons are optional. Here, semicolons are
   required for disambiguation: indeed, in the absence of mandatory
   semicolons, when a sequence begins with x(y,z), it would be unclear whether
   1- x is a parameterized symbol and (y,z) are its actual arguments, or 2- x
   is unparameterized and (y, z) is a tuple pattern which forms the beginning
   of the next element of the sequence. *)

(* We *could* allow the semicolon to be omitted when it precedes an action
   expression (as opposed to a sequence expression). This would be implemented
   in the definition of the nonterminal symbol [continuation]. We choose not
   to do this, as we wish to make it clear in this case that this is a
   sequence whose last element is the action expression. *)

%inline seq_expression:
  e = located(raw_seq_expression)
    { e }

raw_seq_expression:
|                    e1 = symbol_expression e2 = continuation
    { ECons (SemPatWildcard, e1, e2) }
| p1 = pattern EQUAL e1 = symbol_expression e2 = continuation
    { ECons (p1, e1, e2) }
| e = symbol_expression
    { ESingleton e }
| e = action_expression
    { e }

%inline continuation:
  SEMI e2 = seq_expression
(* |   e2 = action_expression *)
    { e2 }

(* A symbol expression takes one of the following forms:

     foo(...)       a terminal or nonterminal symbol (with parameters)
     e*             same as above
     e+             same as above
     e?             same as above *)

(* Note the absence of parenthesized expressions [(e)] in the syntax of symbol
   expressions. There are two reasons why they are omitted. At the syntactic
   level, introducing them would create a conflict. At a semantic level, they
   are both unnecessary and ambiguous, as one can instead write [endrule(e)]
   or [midrule(e)] and thereby indicate whether the anonymous nonterminal
   symbol that is generated should or should not be marked %inline. *)

symbol_expression:
| symbol = symbol es = plist(expression) attrs = ATTRIBUTE*
    { ESymbol (symbol, es, attrs) }
| e = located(symbol_expression) m = located(modifier) attrs = ATTRIBUTE*
    (* We are forced by syntactic considerations to require a symbol expression
       in a position where an expression is expected. As a result, an injection
       must be applied. *)
    { ESymbol (m, [ inject e ], attrs) }

(* An action expression is a semantic action, optionally preceded or followed
   with a precedence annotation. *)

action_expression:
| action = action
    { EAction (action, None) }
| prec = precedence action = action
    { EAction (action, Some prec) }
| action = action prec = precedence
    { EAction (action, Some prec) }

(* A semantic action is either a traditional semantic action (an OCaml
   expression between curly braces) or a point-free semantic action (an
   optional OCaml identifier between angle brackets). *)

(* The token OCAMLTYPE, which until now was supposed to denote an OCaml
   type between angle brackets, is re-used for this purpose. This is not
   very pretty. *)

(* The stretch produced by the lexer is validated -- i.e., we check that
   it contains just an OCaml identifier, or is empty. The parentheses
   added by the lexer to the [stretch_content] field are removed (ugh!)
   because they are problematic when this identifier is a data constructor. *)

action:
  action = ACTION
    { XATraditional action }
| _action = OCAMLTYPE
    { failwith "TODO" }
      (*
        match ParserAux.validate_pointfree_action action with
        | os ->
            XAPointFree (unparenthesize os)
        | exception Lexpointfree.InvalidPointFreeAction ->
            Error.error [Positions.import $loc]
              "A point-free semantic action must consist \
               of a single OCaml identifier." (* or whitespace *)
      *)

(* Patterns. *)

pattern:
| x = LID
    { SemPatVar x }
| UNDERSCORE
    { SemPatWildcard }
| TILDE
    { SemPatTilde (Positions.import $loc) }
| LPAREN ps = separated_list(COMMA, pattern) RPAREN
    { SemPatTuple ps }

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Generic definitions. *)

(* ------------------------------------------------------------------------- *)

(* Formal and actual parameter lists can be absent. When present, they must
   be nonempty, and are delimited with parentheses and separated with commas. *)

%inline plist(X):
  params = loption(delimited(LPAREN, separated_nonempty_list(COMMA, X), RPAREN))
    { params }

(* -------------------------------------------------------------------------- *)

(* [reversed_preceded_or_separated_nonempty_llist(delimiter, X)] recognizes a
   nonempty list of [X]s, separated with [delimiter]s, and optionally preceded
   with a leading [delimiter]. It produces an OCaml list in reverse order. Its
   definition is left-recursive. *)

reversed_preceded_or_separated_nonempty_llist(delimiter, X):
| ioption(delimiter) x = X
    { [x] }
| xs = reversed_preceded_or_separated_nonempty_llist(delimiter, X)
  delimiter
  x = X
    { x :: xs }

(* [preceded_or_separated_nonempty_llist(delimiter, X)] recognizes a nonempty
   list of [X]s, separated with [delimiter]s, and optionally preceded with a
   leading [delimiter]. It produces an OCaml list in direct order. *)

%inline preceded_or_separated_nonempty_llist(delimiter, X):
  xs = rev(reversed_preceded_or_separated_nonempty_llist(delimiter, X))
    { xs }

(* [preceded_or_separated_llist(delimiter, X)] recognizes a possibly empty
   list of [X]s, separated with [delimiter]s, and optionally preceded with a
   leading [delimiter]. It produces an OCaml list in direct order. *)

preceded_or_separated_llist(delimiter, X):
| (* empty *)
    { [] }
| xs = preceded_or_separated_nonempty_llist(delimiter, X)
    { xs }

(* -------------------------------------------------------------------------- *)

(* [located(X)] recognizes the same language as [X] and converts the resulting
   value from type ['a] to type ['a located]. *)

located(X):
  x = X
    { with_loc $loc x }

%%
