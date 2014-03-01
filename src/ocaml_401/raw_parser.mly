
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: parser.mly 12800 2012-07-30 18:59:07Z doligez $ *)

(* The parser definition *)

%{
open Asttypes
open Longident
open Parsetree

let rloc loc_start loc_end =
  { Location. loc_start; loc_end; loc_ghost = false; }
let gloc loc_start loc_end =
  { Location. loc_start; loc_end; loc_ghost = true; }

let mkloc =
  Location.mkloc
let mknoloc =
  Location.mknoloc
let mktyp startpos endpos d =
  { ptyp_desc = d; ptyp_loc = rloc startpos endpos }
let mkpat startpos endpos d =
  { ppat_desc = d; ppat_loc = rloc startpos endpos }
let mkexp startpos endpos d =
  { pexp_desc = d; pexp_loc = rloc startpos endpos }
let mkmty startpos endpos d =
  { pmty_desc = d; pmty_loc = rloc startpos endpos }
let mksig startpos endpos d =
  { psig_desc = d; psig_loc = rloc startpos endpos }
let mkmod startpos endpos d =
  { pmod_desc = d; pmod_loc = rloc startpos endpos }
let mkstr startpos endpos d =
  { pstr_desc = d; pstr_loc = rloc startpos endpos }
let mkfield startpos endpos d =
  { pfield_desc = d; pfield_loc = rloc startpos endpos }
let mkclass startpos endpos d =
  { pcl_desc = d; pcl_loc = rloc startpos endpos }
let mkcty startpos endpos d =
  { pcty_desc = d; pcty_loc = rloc startpos endpos }
let mkctf startpos endpos d =
  { pctf_desc = d; pctf_loc = rloc startpos endpos }
let mkcf startpos endpos d =
  { pcf_desc = d; pcf_loc = rloc startpos endpos }

let mkrhs startpos endpos rhs = mkloc rhs (rloc startpos endpos)

let mkoption d =
  let loc = {d.ptyp_loc with Location.loc_ghost = true} in
  let lident = Ldot (Lident "*predef*", "option") in
  { ptyp_desc = Ptyp_constr (mkloc lident loc,[d]); ptyp_loc = loc }

let reloc_pat startpos endpos x =
  { x with ppat_loc = rloc startpos endpos }
let reloc_exp startpos endpos x =
  { x with pexp_loc = rloc startpos endpos }
let reloc_exp_fake startpos x =
  { x with pexp_loc = Parsing_aux.pack_fake_start x.pexp_loc startpos }

let mkoperator startpos endpos name =
  let loc = rloc startpos endpos in
  { pexp_desc = Pexp_ident (mkloc (Lident name) loc); pexp_loc = loc }

let mkpatvar startpos endpos name =
  let ppat_loc = rloc startpos endpos in
  { ppat_desc = Ppat_var (mkrhs startpos endpos name); ppat_loc; }

let regularize_type_kind = function
  | ((Ptype_variant [] | Ptype_record []), _, None) ->
    (Ptype_abstract, Public, None)
  | ((Ptype_variant [] | Ptype_record []), p, (Some _ as ty)) ->
    (Ptype_abstract, p, ty)
  | tyk -> tyk

(*
  Ghost expressions and patterns:
  expressions and patterns that do not appear explicitly in the
  source file they have the loc_ghost flag set to true.
  Then the profiler will not try to instrument them and the
  -annot option will not try to display their type.

  Every grammar rule that generates an element with a location must
  make at most one non-ghost element, the topmost one.

  How to tell whether your location must be ghost:
  A location corresponds to a range of characters in the source file.
  If the location contains a piece of code that is syntactically
  valid (according to the documentation), and corresponds to the
  AST node, then the location must be real; in all other cases,
  it must be ghost.
*)
let ghexp startpos endpos d =
  { pexp_desc = d; pexp_loc = gloc startpos endpos }
let ghpat startpos endpos d =
  { ppat_desc = d; ppat_loc = gloc startpos endpos }
let ghtyp startpos endpos d =
  { ptyp_desc = d; ptyp_loc = gloc startpos endpos }
let ghloc startpos endpos d =
  { txt = d; loc = gloc startpos endpos }

let mkassert startpos endpos  e =
  match e with
  | {pexp_desc = Pexp_construct ({txt = Lident "false"}, None , false)} ->
    mkexp startpos endpos Pexp_assertfalse
  | _ ->
    mkexp startpos endpos (Pexp_assert e)

let mkinfix startpos endpos arg1 startpos2 endpos2 name arg2 =
  mkexp startpos endpos
    (Pexp_apply (mkoperator startpos2 endpos2 name, ["", arg1; "", arg2]))

let neg_float_string f =
  if String.length f > 0 && f.[0] = '-'
  then String.sub f 1 (String.length f - 1)
  else "-" ^ f

let mkuminus startpos endpos name arg =
  match name, arg.pexp_desc with
  | "-", Pexp_constant (Const_int n) ->
    mkexp startpos endpos (Pexp_constant (Const_int (-n)))
  | "-", Pexp_constant (Const_int32 n) ->
    mkexp startpos endpos (Pexp_constant (Const_int32 (Int32.neg n)))
  | "-", Pexp_constant (Const_int64 n) ->
    mkexp startpos endpos (Pexp_constant (Const_int64 (Int64.neg n)))
  | "-", Pexp_constant (Const_nativeint n) ->
    mkexp startpos endpos (Pexp_constant (Const_nativeint (Nativeint.neg n)))
  | ("-" | "-."), Pexp_constant (Const_float f) ->
    mkexp startpos endpos (Pexp_constant (Const_float (neg_float_string f)))
  | _ ->
    mkexp startpos endpos
      (Pexp_apply (mkoperator startpos endpos ("~" ^ name), ["", arg]))

let mkuplus startpos endpos  name arg =
  let desc = arg.pexp_desc in
  match name, desc with
  | "+", Pexp_constant (Const_int _)
  | "+", Pexp_constant (Const_int32 _)
  | "+", Pexp_constant (Const_int64 _)
  | "+", Pexp_constant (Const_nativeint _)
  | ("+" | "+."), Pexp_constant (Const_float _) ->
    mkexp startpos endpos  desc
  | _ ->
    mkexp startpos endpos
      (Pexp_apply (mkoperator startpos endpos  ("~" ^ name), ["", arg]))

let mkexp_cons consloc args loc =
  let d = Pexp_construct (mkloc (Lident "::") consloc, Some args, false) in
  {pexp_desc = d; pexp_loc = loc}

let mkpat_cons consloc args loc =
  let d = Ppat_construct (mkloc (Lident "::") consloc, Some args, false) in
  {ppat_desc = d; ppat_loc = loc}

let rec mktailexp startpos endpos = function
  | [] -> ghexp startpos endpos
            (Pexp_construct (mkloc (Lident "[]") Location.none, None, false))
  | e1 :: el ->
    let open Location in
    let exp_el = mktailexp e1.pexp_loc.loc_end endpos el in
    let l = gloc e1.pexp_loc.loc_start exp_el.pexp_loc.loc_end in
    let arg = {pexp_desc = Pexp_tuple [e1; exp_el]; pexp_loc = l} in
    mkexp_cons l arg l

let rec mktailpat startpos endpos = function
  | [] -> ghpat startpos endpos
            (Ppat_construct (mkloc (Lident "[]") Location.none, None, false))
  | p1 :: pl ->
    let open Location in
    let pat_pl = mktailpat p1.ppat_loc.loc_end endpos pl in
    let l = gloc p1.ppat_loc.loc_start pat_pl.ppat_loc.loc_end in
    let arg = {ppat_desc = Ppat_tuple [p1; pat_pl]; ppat_loc = l} in
    mkpat_cons l arg l

let mkstrexp e =
  { pstr_desc = Pstr_eval e; pstr_loc = e.pexp_loc }

let array_function startpos endpos str name =
  ghloc startpos endpos
    (Ldot (Lident str, (if Clflags.fast () then "unsafe_" ^ name else name)))

let rec deep_mkrangepat startpos endpos c1 c2 =
  let pat = Ppat_constant (Const_char c1) in
  let pat = ghpat startpos endpos pat in
  if c1 = c2 then
    pat
  else
    ghpat startpos endpos (Ppat_or (pat, deep_mkrangepat startpos endpos
                                      (Char.chr (Char.code c1 + 1)) c2))

let rec mkrangepat startpos endpos c1 c2 =
  if c1 > c2 then mkrangepat startpos endpos c2 c1 else
  if c1 = c2 then mkpat startpos endpos (Ppat_constant (Const_char c1)) else
  reloc_pat startpos endpos (deep_mkrangepat startpos endpos c1 c2)

let syntax_error pos =
  Parsing_aux.raise_warning (Syntaxerr.Escape_error pos)

let unclosed opening_name opstart opend closing_name clstart clend =
  Parsing_aux.raise_warning
    Syntaxerr.(Error (Unclosed (rloc opstart opend, opening_name,
                                rloc clstart clend, closing_name)))

let expecting startpos endpos nonterm =
  Parsing_aux.raise_warning
    Syntaxerr.(Error (Expecting (rloc startpos endpos, nonterm)))

let check_constraint mkexp constr e =
  match constr with
  | (None,None) -> e
  | (t,t') -> mkexp (Pexp_constraint (e, t, t'))

let bigarray_function startpos endpos str name =
  ghloc startpos endpos (Ldot (Ldot (Lident "Bigarray", str), name))

let bigarray_untuplify = function
  | { pexp_desc = Pexp_tuple explist } -> explist
  | exp -> [exp]

let bigarray_get startpos endpos arr arg =
  let get = if Clflags.fast () then "unsafe_get" else "get" in
  let ghexp = ghexp startpos endpos in
  let md, id, args =
    match bigarray_untuplify arg with
    | [c1]       -> "Array1",   get,   ["", c1]
    | [c1;c2]    -> "Array2",   get,   ["", c1; "", c2]
    | [c1;c2;c3] -> "Array3",   get,   ["", c1; "", c2; "", c3]
    | coords     -> "Genarray", "get", ["", ghexp (Pexp_array coords)]
  in
  let path = Pexp_ident (bigarray_function startpos endpos md id) in
  mkexp startpos endpos (Pexp_apply (ghexp path, ["", arr] @ args))

let bigarray_set startpos endpos arr arg newval =
  let set = if Clflags.fast () then "unsafe_set" else "set" in
  let ghexp = ghexp startpos endpos in
  let md, id, args =
    match bigarray_untuplify arg with
    | [c1]       -> "Array1",   set,   ["", c1]
    | [c1;c2]    -> "Array2",   set,   ["", c1; "", c2]
    | [c1;c2;c3] -> "Array3",   set,   ["", c1; "", c2; "", c3]
    | coords     -> "Genarray", "set", ["", ghexp (Pexp_array coords)]
  in
  let path = Pexp_ident (bigarray_function startpos endpos md id) in
  mkexp startpos endpos
    (Pexp_apply (ghexp path, ["", arr] @ args @ ["", newval]))

let lapply startpos endpos p1 p2 =
  if Clflags.applicative_functors () then
    Lapply (p1, p2)
  else
    raise Syntaxerr.(Error (Applicative_path (rloc startpos endpos)))

let exp_of_label startpos endpos lbl =
  mkexp startpos endpos
    (Pexp_ident (mkrhs startpos endpos (Lident (Longident.last lbl))))

let pat_of_label startpos endpos lbl =
  mkpat startpos endpos
    (Ppat_var (mkrhs startpos endpos (Longident.last lbl)))

let check_variable vl loc v =
  if List.mem v vl then
    raise Syntaxerr.(Error (Variable_in_scope (loc,v)))

let varify_constructors var_names t =
  let rec loop t =
    let desc = match t.ptyp_desc with
      | Ptyp_any -> Ptyp_any
      | Ptyp_var x ->
        check_variable var_names t.ptyp_loc x;
        Ptyp_var x
      | Ptyp_arrow (label,core_type,core_type') ->
        Ptyp_arrow (label, loop core_type, loop core_type')
      | Ptyp_tuple lst ->
        Ptyp_tuple (List.map loop lst)
      | Ptyp_constr ({ txt = Lident s}, []) when List.mem s var_names ->
        Ptyp_var s
      | Ptyp_constr (longident, lst) ->
        Ptyp_constr (longident, List.map loop lst)
      | Ptyp_object lst ->
        Ptyp_object (List.map loop_core_field lst)
      | Ptyp_class (longident, lst, lbl_list) ->
        Ptyp_class (longident, List.map loop lst, lbl_list)
      | Ptyp_alias (core_type, string) ->
        check_variable var_names t.ptyp_loc string;
        Ptyp_alias (loop core_type, string)
      | Ptyp_variant (row_field_list, flag, lbl_lst_option) ->
        let row_field_list = List.map loop_row_field row_field_list in
        Ptyp_variant (row_field_list, flag, lbl_lst_option)
      | Ptyp_poly (string_lst, core_type) ->
        List.iter (check_variable var_names t.ptyp_loc) string_lst;
        Ptyp_poly (string_lst, loop core_type)
      | Ptyp_package (longident, lst) ->
        Ptyp_package (longident, List.map (fun (n, typ) -> (n, loop typ)) lst)
    in
    { t with ptyp_desc = desc }
  and loop_core_field t =
    let desc = match t.pfield_desc with
      | Pfield (n, typ) -> Pfield (n, loop typ)
      | Pfield_var      -> Pfield_var
    in
    { t with pfield_desc = desc }
  and loop_row_field  = function
    | Rtag (label,flag,lst) -> Rtag (label, flag, List.map loop lst)
    | Rinherit t            -> Rinherit (loop t)
  in
  loop t

let wrap_type_annotation startpos endpos newtypes core_type body =
  let exp =
    mkexp startpos endpos (Pexp_constraint (body, Some core_type, None))
  in
  let aux newtype exp =
    mkexp startpos endpos (Pexp_newtype (newtype, exp))
  in
  let exp = List.fold_right aux newtypes exp in
  let typ = Ptyp_poly (newtypes, varify_constructors newtypes core_type) in
  (exp, ghtyp startpos endpos typ)

let tag_nonrec (id, a) = (Fake.Nonrec.add id, a)

%}

(* Tokens *)

%token AMPERAMPER
%token AMPERSAND
%token AND
%token AS
%token ASSERT
%token BACKQUOTE
%token BANG
%token BAR
%token BARBAR
%token BARRBRACKET
%token BEGIN
%token <char> CHAR
%token CLASS
%token COLON
%token COLONCOLON
%token COLONEQUAL
%token COLONGREATER
%token COMMA
%token CONSTRAINT
%token DO
%token DONE
%token DOT
%token DOTDOT
%token DOWNTO
%token ELSE
%token END
%token EOF
%token EQUAL
%token EXCEPTION
%token EXTERNAL
%token FALSE
%token <string> FLOAT
%token FOR
%token FUN
%token FUNCTION
%token FUNCTOR
%token GREATER
%token GREATERRBRACE
%token GREATERRBRACKET
%token IF
%token IN
%token INCLUDE
%token <string> INFIXOP0
%token <string> INFIXOP1
%token <string> INFIXOP2
%token <string> INFIXOP3
%token <string> INFIXOP4
%token INHERIT
%token INITIALIZER
%token <int> INT
%token <int32> INT32
%token <int64> INT64
%token <string> LABEL
%token LAZY
%token LBRACE
%token LBRACELESS
%token LBRACKET
%token LBRACKETBAR
%token LBRACKETLESS
%token LBRACKETGREATER
%token LESS
%token LESSMINUS
%token LET
%token <string> LIDENT
%token LPAREN
%token MATCH
%token METHOD
%token MINUS
%token MINUSDOT
%token MINUSGREATER
%token MODULE
%token MUTABLE
%token <nativeint> NATIVEINT
%token NEW
%token OBJECT
%token OF
%token OPEN
%token <string> OPTLABEL
%token OR
(* %token PARSER *)
%token PLUS
%token PLUSDOT
%token <string> PREFIXOP
%token PRIVATE
%token QUESTION
%token QUOTE
%token RBRACE
%token RBRACKET
%token REC
%token NONREC
%token RPAREN
%token RECOVER
%token SEMI
%token SEMISEMI
%token SHARP
%token SIG
%token STAR
%token <string> STRING
%token STRUCT
%token THEN
%token TILDE
%token TO
%token TRUE
%token TRY
%token TYPE
%token <string> UIDENT
%token UNDERSCORE
%token VAL
%token VIRTUAL
%token WHEN
%token WHILE
%token WITH
%token <string * Location.t> COMMENT

%token LET_LWT
%token TRY_LWT
%token MATCH_LWT
%token FINALLY_LWT
%token FOR_LWT
%token WHILE_LWT
%token JSNEW
%token P4_QUOTATION
%token OUNIT_TEST
%token OUNIT_TEST_UNIT
%token OUNIT_TEST_MODULE
%token OUNIT_BENCH
%token OUNIT_BENCH_FUN
%token OUNIT_BENCH_INDEXED
%token OUNIT_BENCH_MODULE

%token ENTRYPOINT

(* Precedences and associativities.

Tokens and rules have precedences.  A reduce/reduce conflict is resolved
in favor of the first rule (in source file order).  A shift/reduce conflict
is resolved by comparing the precedence and associativity of the token to
be shifted with those of the rule to be reduced.

By default, a rule has the precedence of its rightmost terminal (if any).

When there is a shift/reduce conflict between a rule and a token that
have the same precedence, it is resolved using the associativity:
if the token is left-associative, the parser will reduce; if
right-associative, the parser will shift; if non-associative,
the parser will declare a syntax error.

We will only use associativities with operators of the kind  x * x -> x
for example, in the rules of the form    expr: expr BINOP expr
in all other cases, we define two precedences if needed to resolve
conflicts.

The precedences must be listed from low to high.
*)

%nonassoc IN
%nonassoc below_SEMI
%nonassoc SEMI                          (* below EQUAL ({lbl=...; lbl=...}) *)
%nonassoc LET LET_LWT                   (* above SEMI ( ...; let ... in ...) *)
%nonassoc below_WITH
%nonassoc FUNCTION WITH                 (* below BAR  (match ... with ...) *)
%nonassoc FINALLY_LWT
%nonassoc AND             (* above WITH (module rec A: SIG with ... and ...) *)
%nonassoc THEN                          (* below ELSE (if ... then ...) *)
%nonassoc ELSE                          (* (if ... then ... else ...) *)
%nonassoc LESSMINUS                     (* below COLONEQUAL (lbl <- x := e) *)
%right    COLONEQUAL                    (* expr (e := e := e) *)
%nonassoc AS
%left     BAR                           (* pattern (p|p|p) *)
%nonassoc below_COMMA
%left     COMMA                         (* expr/expr_comma_list (e,e,e) *)
%right    MINUSGREATER                  (* core_type2 (t -> t -> t) *)
%right    OR BARBAR                     (* expr (e || e || e) *)
%right    AMPERSAND AMPERAMPER          (* expr (e && e && e) *)
%nonassoc below_EQUAL
%left     INFIXOP0 EQUAL LESS GREATER   (* expr (e OP e OP e) *)
%right    INFIXOP1                      (* expr (e OP e OP e) *)
%right    COLONCOLON                    (* expr (e :: e :: e) *)
%left     INFIXOP2 PLUS PLUSDOT MINUS MINUSDOT  (* expr (e OP e OP e) *)
%left     INFIXOP3 STAR                 (* expr (e OP e OP e) *)
%right    INFIXOP4                      (* expr (e OP e OP e) *)
%nonassoc prec_unary_minus prec_unary_plus (* unary - *)
%nonassoc prec_constant_constructor     (* cf. simple_expr (C versus C x) *)
%nonassoc prec_constr_appl              (* above AS BAR COLONCOLON COMMA *)
%nonassoc SHARP                         (* simple_expr/toplevel_directive *)
%nonassoc below_DOT
%nonassoc DOT

(* Finally, the first tokens of simple_expr are above everything else. *)
%nonassoc BACKQUOTE BANG BEGIN CHAR FALSE FLOAT INT INT32 INT64
          LBRACE LBRACELESS LBRACKET LBRACKETBAR LIDENT LPAREN
          NEW NATIVEINT PREFIXOP STRING TRUE UIDENT P4_QUOTATION JSNEW

(* Entry points *)

(* for implementation files *)
%start implementation
%type <Parsetree.structure> implementation

(* for interface files *)
%start interface
%type <Parsetree.signature> interface

(* merlin: for inline expression *)
%start top_expr
%type <Parsetree.expression> top_expr

%%

(* Entry points *)

implementation:
| ENTRYPOINT str = structure EOF
  { str }

interface:
| ENTRYPOINT sg = signature EOF
  { List.rev sg }

top_expr:
| ENTRYPOINT expr = seq_expr option(SEMISEMI) EOF { expr }

(* Module expressions *)

module_functor_arg:
| LPAREN id = UIDENT COLON mty = module_type RPAREN
  { mkrhs $startpos(id) $endpos(id) id, mty }

module_expr:
| mod_longident
  { mkmod $startpos $endpos (Pmod_ident (mkrhs $startpos($1) $endpos($1) $1)) }
| STRUCT str = structure END
  { mkmod $startpos $endpos (Pmod_structure str) }
| FUNCTOR farg = module_functor_arg MINUSGREATER me = module_expr
  { let id, fty = farg in
    mkmod $startpos $endpos (Pmod_functor (id, fty, me)) }
| module_expr LPAREN module_expr RPAREN
  { mkmod $startpos $endpos (Pmod_apply ($1, $3)) }
| LPAREN module_expr COLON module_type RPAREN
  { mkmod $startpos $endpos (Pmod_constraint ($2, $4)) }
| LPAREN module_expr RPAREN
  { $2 }
| LPAREN VAL expr RPAREN
  { mkmod $startpos $endpos (Pmod_unpack $3) }
| LPAREN VAL expr COLON package_type RPAREN
  { let typ = ghtyp $startpos $endpos (Ptyp_package $5) in
    let exp = Pexp_constraint ($3, Some typ, None) in
    mkmod $startpos $endpos (Pmod_unpack (ghexp $startpos $endpos exp)) }
| LPAREN VAL expr COLON package_type COLONGREATER package_type RPAREN
  { let typ  = ghtyp $startpos $endpos (Ptyp_package $5) in
    let typ' = ghtyp $startpos $endpos (Ptyp_package $7) in
    let exp  = Pexp_constraint ($3, Some typ, Some typ') in
    mkmod $startpos $endpos (Pmod_unpack (ghexp $startpos $endpos exp)) }
| LPAREN VAL expr COLONGREATER package_type RPAREN
  { let typ' = ghtyp $startpos $endpos (Ptyp_package $5) in
    let exp  = Pexp_constraint ($3, None, Some typ') in
    mkmod $startpos $endpos (Pmod_unpack (ghexp $startpos $endpos exp)) }

structure:
| structure_tail
  { $1 }
| expr = seq_expr tl = structure_tail
  { mkstrexp expr :: tl}

structure_tail:
| (*empty*)
  { [] }
| SEMISEMI
  { [] }
| SEMISEMI expr = seq_expr tl = structure_tail
  { mkstrexp expr :: tl }
| SEMISEMI hd = structure_item tl = structure_tail
  { hd @ tl }
| hd = structure_item tl = structure_tail
  { hd @ tl }

with_extensions:
| LIDENT COMMA with_extensions
  { $1 :: $3 }
| LIDENT
  { [$1] }

structure_item:
| RECOVER { [] }
| LET rec_ = rec_flag binds = let_bindings
  { match binds with
    | [{ppat_desc = Ppat_any}, exp] ->
      [mkstr $startpos $endpos (Pstr_eval exp)]
    | [] -> []
    | _ ->
      [mkstr $startpos $endpos (Pstr_value (rec_, List.rev binds))]
  }
| LET_LWT rec_flag let_bindings
  { let v = match $3 with
      | [{ ppat_desc = Ppat_any; ppat_loc = _ }, exp] ->
        [Pstr_eval (Fake.app Fake.Lwt.un_lwt exp)]
      | [] -> []
      | _ ->
        [Pstr_value ($2, List.rev_map (Fake.pat_app Fake.Lwt.un_lwt) $3)]
    in
    List.map (mkstr $startpos $endpos) v
  }
| EXTERNAL val_ident COLON core_type EQUAL primitive_declaration
  { [mkstr $startpos $endpos
      (Pstr_primitive (mkrhs $startpos($2) $endpos($2) $2,
        { pval_type = $4; pval_prim = $6;
          pval_loc = rloc $startpos $endpos }))]
  }
| TYPE type_declarations
  { [mkstr $startpos $endpos (Pstr_type (List.rev $2))] }
| TYPE NONREC type_declarations
  { [mkstr $startpos $endpos (Pstr_type (List.rev_map tag_nonrec $3))] }
| TYPE type_declarations WITH with_extensions
  { let ghost_loc = Some (gloc $startpos($4) $endpos($4)) in
    let ast = Fake.TypeWith.generate_definitions ~ty:($2) ?ghost_loc $4 in
    mkstr $startpos $endpos (Pstr_type (List.rev $2)) :: ast
  }
| TYPE NONREC type_declarations WITH with_extensions
  { let ghost_loc = Some (gloc $startpos($5) $endpos($5)) in
    let ast = Fake.TypeWith.generate_definitions ~ty:($3) ?ghost_loc $5 in
    mkstr $startpos $endpos (Pstr_type (List.rev_map tag_nonrec $3)) :: ast
  }
| EXCEPTION UIDENT constructor_arguments
  { [mkstr $startpos $endpos
      (Pstr_exception (mkrhs $startpos($2) $endpos($2) $2, $3))] }
| EXCEPTION UIDENT constructor_arguments WITH with_extensions
  { [mkstr $startpos $endpos
      (Pstr_exception (mkrhs $startpos($2) $endpos($2) $2, $3))] }
| EXCEPTION UIDENT EQUAL constr_longident
  { [mkstr $startpos $endpos
      (Pstr_exn_rebind (mkrhs $startpos($2) $endpos($2) $2,
                       mkloc $4 (rloc $startpos($4) $endpos($4))))]
  }
| MODULE UIDENT module_binding
  { [mkstr $startpos $endpos
      (Pstr_module (mkrhs $startpos($2) $endpos($2) $2, $3))]
  }
| _m = MODULE _r = REC module_rec_bindings
  { [mkstr $startpos $endpos (Pstr_recmodule (List.rev $3))] }
| MODULE TYPE ident EQUAL module_type
  { [mkstr $startpos $endpos
      (Pstr_modtype (mkrhs $startpos($3) $endpos($3) $3, $5))] }
| OPEN md = mod_open
  { let flag, name = md in
    [mkstr $startpos $endpos (Pstr_open (flag, name))] }
| CLASS class_declarations
  { [mkstr $startpos $endpos
       (Pstr_class (List.rev $2))] }
| CLASS TYPE class_type_declarations
  { [mkstr $startpos $endpos
       (Pstr_class_type (List.rev $3))] }
| INCLUDE module_expr
  { [mkstr $startpos $endpos (Pstr_include $2)] }
| OUNIT_TEST option(STRING) EQUAL seq_expr
  { let expr = Fake.app Fake.OUnit.force_bool $4 in
    [mkstr $startpos $endpos (Pstr_eval expr)]
  }
| OUNIT_TEST_UNIT option(STRING) EQUAL seq_expr
  { let expr = Fake.app Fake.OUnit.force_unit $4 in
    [mkstr $startpos $endpos (Pstr_eval expr)]
  }
| OUNIT_TEST_MODULE option(STRING) EQUAL module_expr
  { let name = Fake.OUnit.fresh_test_module_ident () in
    [mkstr $startpos $endpos
       (Pstr_module (mkrhs $startpos($1) $endpos($2) name, $4))]
  }
| OUNIT_BENCH STRING EQUAL expr = seq_expr
  { [mkstr $startpos $endpos (Pstr_eval expr)] }
| OUNIT_BENCH_FUN STRING EQUAL seq_expr
  { let expr = Fake.app Fake.OUnit.force_unit_arrow_unit $4 in
    [mkstr $startpos $endpos (Pstr_eval expr)] }
| OUNIT_BENCH_INDEXED STRING val_ident simple_expr EQUAL seq_expr
  { let f_arg =
      mkpat $startpos $endpos (Ppat_var (mkrhs $startpos($3) $endpos($3) $3))
    in
    let f_fun =
       mkexp $startpos $endpos (Pexp_function ("", None, [f_arg, $6]))
    in
    let expr = Fake.(app (app OUnit.force_indexed f_fun) $4) in
    [mkstr $startpos $endpos (Pstr_eval expr)]
  }
| OUNIT_BENCH_MODULE STRING EQUAL module_expr
  { let name = Fake.OUnit.fresh_test_module_ident () in
    [mkstr $startpos $endpos
       (Pstr_module (mkrhs $startpos($1) $endpos($2) name, $4))]
  }

module_binding:
| EQUAL module_expr
  { $2 }
| COLON module_type EQUAL module_expr
  { mkmod $startpos $endpos (Pmod_constraint ($4, $2)) }
| LPAREN UIDENT COLON module_type RPAREN module_binding
  { mkmod $startpos $endpos
      (Pmod_functor (mkrhs $startpos($2) $endpos($2) $2, $4, $6)) }

module_rec_bindings:
| module_rec_binding
  { $1 }
| module_rec_bindings AND module_rec_binding
  { $3 @ $1 }

module_rec_binding:
| UIDENT COLON module_type EQUAL module_expr
  { [(mkrhs $startpos($1) $endpos($5) $1, $3, $5)] }

(* Module types *)

module_type:
| mty_longident
  { mkmty $startpos $endpos (Pmty_ident (mkrhs $startpos($1) $endpos($1) $1)) }
| SIG sg = signature END
  { mkmty $startpos $endpos (Pmty_signature (List.rev sg)) }
| FUNCTOR farg = module_functor_arg MINUSGREATER mty = module_type
  %prec below_WITH
  { let id, fty = farg in
     mkmty $startpos $endpos (Pmty_functor (id, mty, fty)) }
| mty = module_type WITH cs = with_constraints
  { match cs with
    | [] -> mty
    | cs -> mkmty $startpos $endpos (Pmty_with (mty, List.rev cs)) }
| MODULE TYPE OF module_expr
  { mkmty $startpos $endpos (Pmty_typeof $4) }
| LPAREN module_type RPAREN
  { $2 }

signature:
| (* empty *)
  { [] }
| tl = signature hd = signature_item option(SEMISEMI)
  { hd @ tl }

signature_item:
| RECOVER { [] }
| VAL id = val_ident COLON pval_type = core_type
  { let t = {pval_type; pval_prim = []; pval_loc = rloc $startpos $endpos} in
    [mksig $startpos $endpos
      (Psig_value (mkrhs $startpos(id) $endpos(id) id, t))] }
| EXTERNAL id = val_ident COLON pval_type = core_type EQUAL p = primitive_declaration
  { let t = {pval_type; pval_prim = p; pval_loc = rloc $startpos $endpos} in
    [mksig $startpos $endpos
      (Psig_value (mkrhs $startpos(id) $endpos(id) id, t))] }
| TYPE type_declarations
  { [ mksig $startpos $endpos (Psig_type (List.rev $2)) ] }
| TYPE NONREC type_declarations
  { [ mksig $startpos $endpos (Psig_type (List.rev_map tag_nonrec $3)) ] }
| TYPE type_declarations WITH with_extensions
  {
    let ghost_loc = Some (gloc $startpos($4) $endpos($4)) in
    let decls = Fake.TypeWith.generate_sigs ~ty:($2) ?ghost_loc $4 in
    List.rev_append decls [mksig $startpos $endpos (Psig_type (List.rev $2))]
  }
| TYPE NONREC type_declarations WITH with_extensions
  {
    let ghost_loc = Some (gloc $startpos($5) $endpos($5)) in
    let decls = Fake.TypeWith.generate_sigs ~ty:($3) ?ghost_loc $5 in
    List.rev_append decls [mksig $startpos $endpos (Psig_type (List.rev_map tag_nonrec $3))]
  }
| EXCEPTION UIDENT constructor_arguments
  { [mksig $startpos $endpos (Psig_exception (mkrhs $startpos($2) $endpos($2) $2, $3))] }
| EXCEPTION UIDENT constructor_arguments WITH with_extensions
  { [mksig $startpos $endpos (Psig_exception (mkrhs $startpos($2) $endpos($2) $2, $3))] }
| MODULE UIDENT module_declaration
  { [mksig $startpos $endpos (Psig_module (mkrhs $startpos($2) $endpos($2) $2, $3))] }
| _m = MODULE _r = REC module_rec_declarations
  (* '_ =' forces MODULE and REC to be on stack as hints for incremental
     analysis *)
  { [mksig $startpos $endpos (Psig_recmodule (List.rev $3))] }
| MODULE TYPE ident
  { [mksig $startpos $endpos (Psig_modtype (mkrhs $startpos($3) $endpos($3) $3, Pmodtype_abstract))] }
| MODULE TYPE ident EQUAL module_type
  { [mksig $startpos $endpos (Psig_modtype (mkrhs $startpos($3) $endpos($3) $3, Pmodtype_manifest $5))] }
| OPEN md = mod_open
  { let flag, name = md in
    [mksig $startpos $endpos (Psig_open (flag, name))] }
| INCLUDE module_type
  { [mksig $startpos $endpos (Psig_include $2)] }
| CLASS class_descriptions
  { [mksig $startpos $endpos (Psig_class (List.rev $2))] }
| CLASS TYPE class_type_declarations
  { [mksig $startpos $endpos (Psig_class_type (List.rev $3))] }


module_declaration:
| COLON module_type
  { $2 }
| LPAREN UIDENT COLON module_type RPAREN module_declaration
  { mkmty $startpos $endpos (Pmty_functor (mkrhs $startpos($2) $endpos($2) $2, $4, $6)) }

module_rec_declarations:
| module_rec_declaration
  { $1 }
| module_rec_declarations AND module_rec_declaration
  { $3 @ $1 }

module_rec_declaration:
| id = UIDENT COLON ty = module_type
  { [mkrhs $startpos(id) $endpos(id) id, ty] }


(* Class expressions *)

class_declarations:
| class_declarations AND class_declaration
  { $3 @ $1 }
| class_declaration
  { $1 }

class_declaration:
| v = virtual_flag p = class_type_parameters id = LIDENT d = class_fun_binding
  { let params, variance = List.split (fst p) in
    [{ pci_virt = v;
      pci_params = params, snd p;
      pci_name = mkrhs $startpos(id) $endpos(id) id;
      pci_expr = d;
      pci_variance = variance;
      pci_loc = rloc $startpos $endpos
    }]
  }

class_fun_binding:
| EQUAL e = class_expr
  { e }
| COLON t = class_type EQUAL e = class_expr
  { mkclass $startpos $endpos (Pcl_constraint (e, t)) }
| p = labeled_simple_pattern e = class_fun_binding
  { let (l,o,p) = p in mkclass $startpos $endpos (Pcl_fun (l, o, p, e)) }

class_type_parameters:
| (*empty*)
  { [], gloc Lexing.dummy_pos Lexing.dummy_pos }
| LBRACKET type_parameter_list RBRACKET
  { List.rev $2, rloc $startpos $endpos }

class_fun_def:
| labeled_simple_pattern MINUSGREATER class_expr
  { let (l,o,p) = $1 in mkclass $startpos $endpos (Pcl_fun (l, o, p, $3)) }
| labeled_simple_pattern class_fun_def
  { let (l,o,p) = $1 in mkclass $startpos $endpos (Pcl_fun (l, o, p, $2)) }

class_expr:
| class_simple_expr
  { $1 }
| FUN class_fun_def
  { $2 }
| expr = class_simple_expr args = simple_labeled_expr_list
  { match List.rev args with
    | [] -> expr
    | args -> mkclass $startpos $endpos (Pcl_apply (expr, List.rev args)) }
| LET rec_ = rec_flag binds = let_bindings IN expr = class_expr
  { match List.rev binds with
    | [] -> expr
    | binds -> mkclass $startpos $endpos (Pcl_let (rec_, binds, expr)) }

class_simple_expr:
| LBRACKET core_type_comma_list RBRACKET class_longident
  { mkclass $startpos $endpos
      (Pcl_constr (mkloc $4 (rloc $startpos($4) $endpos($4)), List.rev $2)) }
| class_longident
  { mkclass $startpos $endpos
      (Pcl_constr (mkrhs $startpos($1) $endpos($1) $1, [])) }
| _o = OBJECT cstr = class_structure END
  { mkclass $startpos $endpos (Pcl_structure cstr) }
| LPAREN class_expr COLON class_type RPAREN
  { mkclass $startpos $endpos (Pcl_constraint ($2, $4)) }
| LPAREN class_expr RPAREN
  { $2 }

class_structure:
| class_self_pattern class_fields
  { { pcstr_pat = $1; pcstr_fields = List.rev $2 } }

class_self_pattern:
| LPAREN pattern RPAREN
  { reloc_pat $startpos $endpos $2 }
| LPAREN pattern COLON core_type RPAREN
  { mkpat $startpos $endpos (Ppat_constraint ($2, $4)) }
| (* empty *)
  { ghpat $startpos $endpos Ppat_any }

class_fields:
| (* empty *)
  { [] }
| class_fields class_field
  { $2 @ $1 }

class_field:
| INHERIT override_flag class_expr parent_binder
  { [mkcf $startpos $endpos  (Pcf_inher ($2, $3, $4))] }
| VAL v = virtual_value
  { match v with
    | None -> []
    | Some v -> [mkcf $startpos $endpos (Pcf_valvirt v)]
  }
| VAL v = value
  { match v with
    | Some v -> [mkcf $startpos $endpos (Pcf_val v)]
    | None -> []
  }
| m = virtual_method
  { match m with
    | Some m -> [mkcf $startpos $endpos (Pcf_virt m)]
    | None -> []
  }
| m = concrete_method
  { match m with
    | Some m -> [mkcf $startpos $endpos (Pcf_meth m)]
    | None -> []
  }
| CONSTRAINT cf = constrain_field
  { match cf with
    | None -> []
    | Some cf -> [mkcf $startpos $endpos  (Pcf_constr cf)]
  }
| INITIALIZER seq_expr
  { [mkcf $startpos $endpos  (Pcf_init $2)] }

parent_binder:
| AS id = LIDENT
  { Some id }
| (* empty *)
  { None }

virtual_value:
| override_flag MUTABLE VIRTUAL label COLON core_type
  { if $1 = Override then syntax_error $startpos($1);
    Some (mkloc $4 (rloc $startpos($4) $endpos($4)), Mutable, $6) }
| VIRTUAL mutable_flag label COLON core_type
  { Some (mkrhs $startpos($3) $endpos($3) $3, $2, $5) }

value:
| override_flag mutable_flag label EQUAL seq_expr
  { Some (mkrhs $startpos($3) $endpos($3) $3, $2, $1, $5) }
| override_flag mutable_flag label type_constraint EQUAL seq_expr
  { Some (mkrhs $startpos($3) $endpos($3) $3, $2, $1,
          check_constraint (ghexp $startpos $endpos) $4 $6) }

virtual_method:
| METHOD override_flag PRIVATE VIRTUAL label COLON poly_type
  { if $2 = Override then syntax_error $startpos($2);
    Some (mkloc $5 (rloc $startpos($5) $endpos($5)), Private, $7) }
| METHOD override_flag VIRTUAL private_flag label COLON poly_type
  { if $2 = Override then syntax_error $startpos($2);
    Some (mkloc $5 (rloc $startpos($5) $endpos($5)), $4, $7) }

concrete_method:
| METHOD override_flag private_flag label strict_binding
  { Some (mkloc $4 (rloc $startpos($4) $endpos($4)), $3, $2, ghexp $startpos $endpos (Pexp_poly ($5, None))) }
| METHOD override_flag private_flag label COLON poly_type EQUAL seq_expr
  { Some (mkloc $4 (rloc $startpos($4) $endpos($4)), $3, $2, ghexp $startpos $endpos (Pexp_poly ($8,Some $6))) }
| METHOD override_flag private_flag label COLON TYPE lident_list
  DOT core_type EQUAL seq_expr
  { let exp, poly = wrap_type_annotation $startpos($6) $endpos($9) $7 $9 $11 in
    Some (mkloc $4 (rloc $startpos($4) $endpos($4)), $3, $2, ghexp $startpos $endpos (Pexp_poly (exp, Some poly))) }


(* Class types *)

class_type:
| class_signature
  { $1 }
| QUESTION LIDENT COLON simple_core_type_or_tuple MINUSGREATER class_type
  { mkcty $startpos $endpos (Pcty_fun ("?" ^ $2 , mkoption  $4, $6)) }
| OPTLABEL simple_core_type_or_tuple MINUSGREATER class_type
  { mkcty $startpos $endpos (Pcty_fun ("?" ^ $1, mkoption  $2, $4)) }
| LIDENT COLON simple_core_type_or_tuple MINUSGREATER class_type
  { mkcty $startpos $endpos (Pcty_fun ($1, $3, $5)) }
| simple_core_type_or_tuple MINUSGREATER class_type
  { mkcty $startpos $endpos (Pcty_fun ("", $1, $3)) }

class_signature:
| LBRACKET core_type_comma_list RBRACKET clty_longident
  { mkcty $startpos $endpos
      (Pcty_constr (mkloc $4 (rloc $startpos($4) $endpos($4)), List.rev $2)) }
| clty_longident
  { mkcty $startpos $endpos (Pcty_constr (mkrhs $startpos($1) $endpos($1) $1, [])) }
| OBJECT csg = class_sig_body END
  { mkcty $startpos $endpos (Pcty_signature csg) }

class_sig_body:
| class_self_type class_sig_fields
  { { pcsig_self = $1; pcsig_fields = List.rev $2;
    pcsig_loc = rloc $startpos $endpos ; } }

class_self_type:
| LPAREN core_type RPAREN
  { $2 }
| (* empty *)
  { mktyp $startpos $endpos Ptyp_any }

class_sig_fields:
| (* empty *)
  { [] }
| class_sig_fields class_sig_field
  { $2 @ $1 }

class_sig_field:
| INHERIT class_signature
  { [mkctf $startpos $endpos (Pctf_inher $2)] }
| VAL vt = value_type
  { match vt with
    | None -> []
    | Some vt -> [mkctf $startpos $endpos (Pctf_val vt)] }
| mt = virtual_method_type
  { match mt with
    | None -> []
    | Some mt -> [mkctf $startpos $endpos  (Pctf_virt mt)] }
| mt = method_type
  { match mt with
    | None -> []
    | Some mt -> [mkctf $startpos $endpos (Pctf_meth mt)] }
| CONSTRAINT cf = constrain_field
  { match cf with
    | None -> []
    | Some cf -> [mkctf $startpos $endpos  (Pctf_cstr cf)]
  }

value_type:
| VIRTUAL mutable_flag label COLON core_type
  { Some ($3, $2, Virtual, $5) }
| MUTABLE virtual_flag label COLON core_type
  { Some ($3, Mutable, $2, $5) }
| label COLON core_type
  { Some ($1, Immutable, Concrete, $3) }

method_type:
| METHOD private_flag label COLON poly_type
  { Some ($3, $2, $5) }

virtual_method_type:
| METHOD PRIVATE VIRTUAL label COLON poly_type
  { Some ($4, Private, $6) }
| METHOD VIRTUAL private_flag label COLON poly_type
  { Some ($4, $3, $6) }

constrain:
| core_type EQUAL core_type
  { [$1, $3, rloc $startpos $endpos]  }

constrain_field:
| core_type EQUAL core_type
  { Some ($1, $3) }

class_descriptions:
| class_descriptions AND class_description
  { $3 @ $1 }
| class_description
  { $1 }

class_description:
| virtual_flag class_type_parameters LIDENT COLON class_type
  { let params, variance = List.split (fst $2) in
    [{ pci_virt = $1;
      pci_params = params, snd $2;
      pci_name = mkrhs $startpos($3) $endpos($3) $3;
      pci_expr = $5;
      pci_variance = variance;
      pci_loc = rloc $startpos $endpos
    }]
  }

class_type_declarations:
| class_type_declarations AND class_type_declaration
  { $3 @ $1 }
| class_type_declaration
  { $1 }

class_type_declaration:
| virtual_flag class_type_parameters LIDENT EQUAL class_signature
  { let params, variance = List.split (fst $2) in
    [{ pci_virt = $1;
      pci_params = params, snd $2;
      pci_name = mkrhs $startpos($3) $endpos($3) $3;
      pci_expr = $5;
      pci_variance = variance;
      pci_loc = rloc $startpos $endpos;
     }]
  }

(* Core expressions *)

seq_expr:
| expr %prec below_SEMI
  { $1 }
| expr SEMI
  { reloc_exp $startpos $endpos $1 }
| expr SEMI seq_expr
  { mkexp $startpos $endpos (Pexp_sequence ($1, $3)) }

labeled_simple_pattern:
| QUESTION LPAREN label_let_pattern opt_default RPAREN
  { match $4 with
    | None -> ("?" ^ fst $3, None, reloc_pat $startpos $endpos (snd $3))
    | Some e ->
      let e' = reloc_exp $startpos($4) $endpos e in
      ("?" ^ fst $3, Some e', reloc_pat $startpos $endpos($3) (snd $3))
  }
| QUESTION label_var
  { ("?" ^ fst $2, None, reloc_pat $startpos $endpos (snd $2)) }
| OPTLABEL LPAREN let_pattern opt_default RPAREN
  { match $4 with
    | None -> ("?" ^ $1, None, reloc_pat $startpos $endpos $3)
    | Some e ->
      let e' = reloc_exp $startpos($4) $endpos e in
      ("?" ^ $1, Some e', reloc_pat $startpos $endpos($3) $3)
  }
| OPTLABEL pattern_var
  { ("?" ^ $1, None, reloc_pat $startpos $endpos $2) }
| TILDE LPAREN label_let_pattern RPAREN
  { (fst $3, None, reloc_pat $startpos $endpos (snd $3)) }
| TILDE label_var
  { (fst $2, None, reloc_pat $startpos $endpos (snd $2)) }
| LABEL simple_pattern
  { ($1, None, reloc_pat $startpos $endpos $2) }
| simple_pattern
  { ("", None, $1) }

pattern_var:
| LIDENT
  { mkpat $startpos $endpos (Ppat_var (mkrhs $startpos($1) $endpos($1) $1)) }
| UNDERSCORE
  { mkpat $startpos $endpos Ppat_any }

opt_default:
| (* empty *)
  { None }
| EQUAL seq_expr
  { Some $2 }

label_let_pattern:
| label_var
  { $1 }
| label_var COLON core_type
  { let (lab, pat) = $1 in
    (lab, mkpat $startpos $endpos (Ppat_constraint (pat, $3))) }

label_var:
| LIDENT
  { ($1, mkpat $startpos $endpos
           (Ppat_var (mkrhs $startpos($1) $endpos($1) $1))) }

let_pattern:
| pattern
  { $1 }
| pattern COLON core_type
  { mkpat $startpos $endpos (Ppat_constraint ($1, $3)) }

expr:
  simple_expr
  { $1 }
| simple_expr simple_labeled_expr_list
  { mkexp $startpos $endpos (Pexp_apply ($1, List.rev $2)) }
| LET rec_ = rec_flag binds = let_bindings _in = IN expr = seq_expr
  { let expr = reloc_exp_fake $endpos(_in) expr in
    mkexp $startpos $endpos (Pexp_let (rec_, List.rev binds, expr)) }
| LET_LWT rec_flag let_bindings IN seq_expr
  { let expr = reloc_exp_fake $endpos($4) $5 in
    let expr = Pexp_let ($2, List.rev_map (Fake.pat_app Fake.Lwt.un_lwt) $3, expr) in
    Fake.app Fake.Lwt.in_lwt (mkexp $startpos $endpos expr) }
| LET MODULE UIDENT module_binding IN seq_expr
  { let expr = reloc_exp_fake $endpos($5) $6 in
    mkexp $startpos $endpos (Pexp_letmodule (mkrhs $startpos($3) $endpos($3) $3, $4, expr)) }
| LET OPEN md = mod_open _in = IN expr = seq_expr
  { let flag, name = md in
    let expr = reloc_exp_fake $endpos(_in) expr in
    mkexp $startpos $endpos (Pexp_open (flag, name, expr)) }
| FUNCTION option(BAR) match_cases
  { mkexp $startpos $endpos (Pexp_function ("", None, List.rev $3)) }
| FUN labeled_simple_pattern fun_def
  { let (l,o,p) = $2 in mkexp $startpos $endpos (Pexp_function (l, o, [p, $3])) }
| FUN new_type fun_def
  { mkexp $startpos $endpos (Pexp_newtype ($2, $3)) }
| MATCH seq_expr WITH option(BAR) match_cases
  { mkexp $startpos $endpos (Pexp_match ($2, List.rev $5)) }
| MATCH_LWT seq_expr WITH option(BAR) match_cases
  { let expr = mkexp $startpos $endpos
        (Pexp_match (Fake.app Fake.Lwt.un_lwt $2, List.rev $5)) in
      Fake.app Fake.Lwt.in_lwt expr }
| TRY seq_expr WITH option(BAR) match_cases
  { mkexp $startpos $endpos (Pexp_try ($2, List.rev $5)) }
| TRY_LWT seq_expr %prec below_WITH
  { reloc_exp $startpos $endpos (Fake.app Fake.Lwt.in_lwt $2) }
| TRY_LWT seq_expr WITH option(BAR) match_cases
  { mkexp $startpos $endpos (Pexp_try (Fake.app Fake.Lwt.in_lwt $2, List.rev $5)) }
| TRY_LWT seq_expr FINALLY_LWT seq_expr
  { Fake.app (Fake.app Fake.Lwt.finally' $2) $4 }
| TRY_LWT seq_expr WITH option(BAR) match_cases FINALLY_LWT seq_expr
  { let expr = mkexp $startpos $endpos
        (Pexp_try (Fake.app Fake.Lwt.in_lwt $2, List.rev $5)) in
      Fake.app (Fake.app Fake.Lwt.finally' expr) $7 }
| expr_comma_list %prec below_COMMA
  { mkexp $startpos $endpos (Pexp_tuple (List.rev $1)) }
| constr_longident simple_expr
  { mkexp $startpos $endpos (Pexp_construct (mkrhs $startpos($1) $endpos($1) $1, Some $2, false)) }
| name_tag simple_expr
  { mkexp $startpos $endpos (Pexp_variant ($1, Some $2)) }
| IF seq_expr THEN expr ELSE expr
  { mkexp $startpos $endpos (Pexp_ifthenelse ($2, $4, Some $6)) }
| IF seq_expr THEN expr
  { mkexp $startpos $endpos (Pexp_ifthenelse ($2, $4, None)) }
| WHILE seq_expr DO seq_expr DONE
  { mkexp $startpos $endpos (Pexp_while ($2, $4)) }
| WHILE_LWT seq_expr DO seq_expr DONE
  { let expr = Pexp_while ($2, Fake.(app Lwt.un_lwt $4)) in
    Fake.(app Lwt.to_lwt (mkexp $startpos $endpos expr)) }
| FOR val_ident EQUAL seq_expr direction_flag seq_expr DO seq_expr DONE
  { mkexp $startpos $endpos (Pexp_for (mkrhs $startpos($2) $endpos($2) $2, $4, $6, $5, $8)) }
| FOR_LWT val_ident EQUAL seq_expr direction_flag seq_expr DO seq_expr DONE
  { let expr =
      Pexp_for (mkrhs $startpos($2) $endpos($2) $2,
                $4, $6, $5, Fake.(app Lwt.un_lwt $8))
    in
    Fake.(app Lwt.to_lwt (mkexp $startpos $endpos expr)) }
| FOR_LWT pattern IN seq_expr DO seq_expr DONE
  { mkexp $startpos $endpos
        (Pexp_let (Nonrecursive, [$2,Fake.(app Lwt.un_stream $4)], Fake.(app Lwt.unit_lwt $6))) }
| expr COLONCOLON expr
  { mkexp_cons (rloc $startpos($2) $endpos($2))
                 (ghexp $startpos $endpos (Pexp_tuple[$1;$3]))
                 (rloc $startpos $endpos) }
| LPAREN COLONCOLON RPAREN LPAREN expr COMMA expr RPAREN
  { mkexp_cons (rloc $startpos($2) $endpos($2))
                 (ghexp $startpos $endpos (Pexp_tuple[$5;$7]))
                 (rloc $startpos $endpos) }
| expr INFIXOP0 expr
  { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) $2 $3 }
| expr INFIXOP1 expr
  { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) $2 $3 }
| expr INFIXOP2 expr
  { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) $2 $3 }
| expr INFIXOP3 expr
  { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) $2 $3 }
| expr INFIXOP4 expr
  { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) $2 $3 }
| expr PLUS expr
  { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "+" $3 }
| expr PLUSDOT expr
  { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "+." $3 }
| expr MINUS expr
  { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "-" $3 }
| expr MINUSDOT expr
  { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "-." $3 }
| expr STAR expr
  { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "*" $3 }
| expr EQUAL expr
  { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "=" $3 }
| expr LESS expr
  { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "<" $3 }
| expr GREATER expr
  { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) ">" $3 }
| expr OR expr
  { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "or" $3 }
| expr BARBAR expr
  { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "||" $3 }
| expr AMPERSAND expr
  { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "&" $3 }
| expr AMPERAMPER expr
  { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) "&&" $3 }
| expr COLONEQUAL expr
  { mkinfix $startpos $endpos $1 $startpos($2) $endpos($2) ":=" $3 }
| subtractive expr %prec prec_unary_minus
  { mkuminus $startpos $endpos $1 $2 }
| additive expr %prec prec_unary_plus
  { mkuplus $startpos $endpos $1 $2 }
| simple_expr DOT label_longident LESSMINUS expr
  { mkexp $startpos $endpos (Pexp_setfield ($1, mkrhs $startpos($3) $endpos($3) $3, $5)) }
| simple_expr DOT LPAREN seq_expr RPAREN LESSMINUS expr
  { mkexp $startpos $endpos
        (Pexp_apply (ghexp $startpos $endpos (Pexp_ident (array_function $startpos $endpos "Array" "set")),
                    ["",$1; "",$4; "",$7])) }
| simple_expr DOT LBRACKET seq_expr RBRACKET LESSMINUS expr
  { mkexp $startpos $endpos
        (Pexp_apply (ghexp $startpos $endpos (Pexp_ident (array_function $startpos $endpos "String" "set")),
                    ["",$1; "",$4; "",$7])) }
| simple_expr DOT LBRACE expr RBRACE LESSMINUS expr
  { bigarray_set $startpos($1) $endpos($7) $1 $4 $7 }

| simple_expr SHARP SHARP label
  { let inst = Fake.(app Js.un_js $1) in
      let field = mkexp $startpos $endpos (Pexp_send (inst, $4)) in
      let prop = Fake.(app Js.un_prop field) in
      mkexp $startpos $endpos (Pexp_send (prop,"get"))
    }
| simple_expr SHARP SHARP label LESSMINUS expr
  { let inst = Fake.(app Js.un_js $1) in
      let field = mkexp $startpos $endpos($4) (Pexp_send (inst, $4)) in
      let prop = Fake.(app Js.un_prop field) in
      let setter = mkexp $startpos $endpos($4) (Pexp_send (prop,"set")) in
      reloc_exp $startpos $endpos
      Fake.(app setter $6)
    }
| simple_expr SHARP SHARP label LPAREN RPAREN
  { let inst = Fake.(app Js.un_js $1) in
      let jsmeth = mkexp $startpos $endpos($4) (Pexp_send (inst, $4)) in
      Fake.(app Js.un_meth jsmeth)
    }
| simple_expr SHARP SHARP error
  { syntax_error $startpos($4);
      let inst = Fake.(app Js.un_js $1) in
      let jsmeth = mkexp $startpos $endpos($4) (Pexp_send (inst, "")) in
      Fake.(app Js.un_meth jsmeth)
    }
| simple_expr SHARP SHARP label LPAREN expr_comma_opt_list RPAREN
  { let inst = Fake.(app Js.un_js $1) in
      let meth = mkexp $startpos $endpos($4) (Pexp_send (inst, $4)) in
      let jsmeth =
        List.fold_left
          (fun meth arg ->
            reloc_exp meth.pexp_loc.Location.loc_start
                      arg.pexp_loc.Location.loc_end
            (Fake.app meth arg))
          meth (List.rev $6)
      in
      Fake.(app Js.un_meth jsmeth)
    }

| label LESSMINUS expr
  { mkexp $startpos $endpos (Pexp_setinstvar (mkrhs $startpos($1) $endpos($1) $1, $3)) }
| ASSERT simple_expr
  { mkassert $startpos $endpos $2 }
| LAZY simple_expr
  { mkexp $startpos $endpos (Pexp_lazy ($2)) }
| OBJECT class_structure END
  { mkexp $startpos $endpos (Pexp_object ($2)) }

simple_expr:
  val_longident
  { mkexp $startpos $endpos (Pexp_ident (mkrhs $startpos($1) $endpos($1) $1)) }
| constant
  { mkexp $startpos $endpos (Pexp_constant $1) }
| constr_longident %prec prec_constant_constructor
  { mkexp $startpos $endpos (Pexp_construct (mkrhs $startpos($1) $endpos($1) $1, None, false)) }
| name_tag %prec prec_constant_constructor
  { mkexp $startpos $endpos (Pexp_variant ($1, None)) }
| LPAREN seq_expr RPAREN
  { reloc_exp $startpos $endpos $2 }
| BEGIN seq_expr END
  { reloc_exp $startpos $endpos $2 }
| BEGIN END
  { mkexp $startpos $endpos (Pexp_construct (mkloc (Lident "()") (rloc $startpos $endpos),
                           None, false)) }
| LPAREN e = constrained_seq_expr RPAREN
  { reloc_exp $startpos $endpos e }
| simple_expr DOT label_longident
  { mkexp $startpos $endpos (Pexp_field ($1, mkrhs $startpos($3) $endpos($3) $3)) }
| mod_longident DOT LPAREN seq_expr RPAREN
  { mkexp $startpos $endpos (Pexp_open (Fresh, mkrhs $startpos($1) $endpos($1) $1, $4)) }
| mod_longident DOT LPAREN EOF
  { syntax_error $startpos($4);
      mkexp $startpos $endpos (Pexp_open (Fresh, mkrhs $startpos($1) $endpos($1) $1, reloc_exp $startpos($3) $endpos($4) Fake.any_val')) }
| mod_longident DOT LPAREN RPAREN
  { syntax_error $startpos($4);
      mkexp $startpos $endpos (Pexp_open (Fresh, mkrhs $startpos($1) $endpos($1) $1, reloc_exp $startpos($3) $endpos($4) Fake.any_val')) }
| simple_expr DOT LPAREN seq_expr RPAREN
  { mkexp $startpos $endpos (Pexp_apply (ghexp $startpos $endpos (Pexp_ident (array_function $startpos $endpos "Array" "get")),
                       ["",$1; "",$4])) }

| simple_expr DOT LBRACKET seq_expr RBRACKET
  { mkexp $startpos $endpos (Pexp_apply (ghexp $startpos $endpos (Pexp_ident (array_function $startpos $endpos "String" "get")),
                       ["",$1; "",$4])) }
| simple_expr DOT LBRACE expr RBRACE
  { bigarray_get $startpos $endpos $1 $4 }
| LBRACE record_expr RBRACE
  { let (exten, fields) = $2 in mkexp $startpos $endpos (Pexp_record (fields, exten)) }
| LBRACKETBAR expr_semi_list option(SEMI) BARRBRACKET
  { mkexp $startpos $endpos (Pexp_array (List.rev $2)) }
| LBRACKETBAR BARRBRACKET
  { mkexp $startpos $endpos (Pexp_array []) }
| LBRACKET expr_semi_list option(SEMI) RBRACKET
  { reloc_exp $startpos $endpos (mktailexp $startpos $endpos (List.rev $2)) }
| PREFIXOP simple_expr
  { mkexp $startpos $endpos (Pexp_apply (mkoperator $startpos($1) $endpos($1) $1, ["",$2])) }
| BANG simple_expr
  { mkexp $startpos $endpos (Pexp_apply (mkoperator $startpos($1) $endpos($1) "!", ["",$2])) }
| NEW class_longident
  { mkexp $startpos $endpos (Pexp_new (mkrhs $startpos($2) $endpos($2) $2)) }
| LBRACELESS field_expr_list option(SEMI) GREATERRBRACE
  { mkexp $startpos $endpos (Pexp_override (List.rev $2)) }
| LBRACELESS GREATERRBRACE
  { mkexp $startpos $endpos (Pexp_override []) }
| simple_expr SHARP label
  { mkexp $startpos $endpos (Pexp_send ($1, $3)) }
| LPAREN MODULE module_expr RPAREN
  { mkexp $startpos $endpos (Pexp_pack $3) }
| LPAREN MODULE module_expr COLON package_type RPAREN
  { mkexp $startpos $endpos
      (Pexp_constraint (ghexp $startpos $endpos (Pexp_pack $3),
                       Some (ghtyp $startpos $endpos  (Ptyp_package $5)), None)) }
(* CamlP4 compatibility *)
| P4_QUOTATION
  { reloc_exp $startpos $endpos Fake.any_val' }
(* Js_of_ocaml extension *)
| JSNEW simple_expr LPAREN RPAREN
  { reloc_exp $startpos $endpos
    Fake.(app Js.un_constr $2)
  }
| JSNEW simple_expr LPAREN expr_comma_opt_list RPAREN
  { let jsnew' = reloc_exp $startpos($1) $endpos($1) Fake.Js.un_constr in
    let constr = reloc_exp $startpos($1) $endpos($2) Fake.(app jsnew' $2) in
    reloc_exp $startpos $endpos
    (List.fold_left
       (fun constr arg ->
         reloc_exp constr.pexp_loc.Location.loc_start
                   arg.pexp_loc.Location.loc_end
         (Fake.app constr arg))
       constr (List.rev $4))
  }

simple_labeled_expr_list:
| labeled_simple_expr
  { [$1] }
| simple_labeled_expr_list labeled_simple_expr
  { $2 :: $1 }

labeled_simple_expr:
| simple_expr
  { ("", $1) }
| label_expr
  { $1 }

label_expr:
| LABEL simple_expr
  { ($1, reloc_exp $startpos $endpos $2) }
| TILDE label_ident
  { (fst $2, reloc_exp $startpos $endpos (snd $2)) }
| QUESTION label_ident
  { ("?" ^ fst $2, reloc_exp $startpos $endpos (snd $2)) }
| OPTLABEL simple_expr
  { ("?" ^ $1, reloc_exp $startpos $endpos $2) }

label_ident:
| LIDENT
  { ($1, mkexp $startpos $endpos (Pexp_ident (mkrhs $startpos($1) $endpos($1) (Lident $1)))) }

let_bindings:
| let_binding
  { $1 }
| let_bindings AND let_binding
  { $3 @ $1 }

lident_list:
| LIDENT
  { [$1] }
| LIDENT lident_list
  { $1 :: $2 }

let_binding:
| val_ident fun_binding
  { [mkpatvar $startpos($1) $endpos($1) $1, $2] }
| val_ident COLON typevar_list DOT core_type EQUAL seq_expr
  { [ghpat $startpos($1) $endpos($5)
      (Ppat_constraint (mkpatvar $startpos($1) $endpos($1) $1,
                       ghtyp $startpos($3) $endpos($5) (Ptyp_poly ($3,$5)))), $7] }
| val_ident COLON TYPE lident_list DOT core_type EQUAL seq_expr
  { let exp, poly = wrap_type_annotation $startpos($3) $endpos($6) $4 $6 $8 in
    [ghpat $startpos($1) $endpos($6) (Ppat_constraint (mkpatvar $startpos($1) $endpos($1) $1, poly)), exp] }
| pattern EQUAL seq_expr
  { [$1, $3] }

fun_binding:
| strict_binding
  { $1 }
| type_constraint EQUAL seq_expr
  { check_constraint (ghexp $startpos $endpos) $1 $3 }

strict_binding:
| EQUAL seq_expr
  { $2 }
| labeled_simple_pattern fun_binding
  { let (l, o, p) = $1 in ghexp $startpos $endpos (Pexp_function (l, o, [p, $2])) }
| new_type fun_binding
  { mkexp $startpos $endpos (Pexp_newtype ($1, $2)) }

match_cases:
| pattern match_action
  { [$1, $2] }
| match_cases BAR pattern match_action
  { ($3, $4) :: $1 }

fun_def:
| match_action
  { $1 }
| labeled_simple_pattern fun_def
  { let (l,o,p) = $1 in ghexp $startpos $endpos (Pexp_function (l, o, [p, $2])) }
| new_type fun_def
  { mkexp $startpos $endpos (Pexp_newtype ($1, $2)) }

match_action:
| MINUSGREATER seq_expr
  { $2 }
| WHEN seq_expr MINUSGREATER seq_expr
  { ghexp $startpos $endpos (Pexp_when ($2, $4)) }

expr_comma_list:
| expr_comma_list COMMA expr
  { $3 :: $1 }
| expr COMMA expr
  { [$3; $1] }

expr_comma_opt_list:
| expr_comma_opt_list COMMA expr
  { $3 :: $1 }
| expr %prec COMMA
  { [$1] }

record_expr:
| simple_expr WITH lbl_expr_list
  { (Some $1, $3) }
| lbl_expr_list
  { (None, $1) }

lbl_expr_list:
| lbl_expr
  { $1 }
| lbl_expr SEMI lbl_expr_list
  { $1 @ $3 }
| lbl_expr SEMI
  { $1 }

lbl_expr:
| label_longident EQUAL expr
  { [mkrhs $startpos($1) $endpos($1) $1,$3] }
| label_longident
  { [mkrhs $startpos($1) $endpos($1) $1,
     exp_of_label $startpos($1) $endpos($1) $1] }

field_expr_list:
| label EQUAL expr
  { [mkrhs $startpos($1) $endpos($1) $1,$3] }
| field_expr_list SEMI label EQUAL expr
  { (mkrhs $startpos($3) $endpos($3) $3, $5) :: $1 }

expr_semi_list:
| expr
  { [$1] }
| expr_semi_list SEMI expr
  { $3 :: $1 }

type_constraint:
| COLON core_type
  { (Some $2, None) }
| COLON core_type COLONGREATER core_type
  { (Some $2, Some $4) }
| COLONGREATER core_type
  { (None, Some $2) }

(* Patterns *)

pattern:
| simple_pattern
  { $1 }
| pattern AS val_ident
  { mkpat $startpos $endpos (Ppat_alias ($1, mkrhs $startpos($3) $endpos($3) $3)) }
| pattern_comma_list  %prec below_COMMA
  { mkpat $startpos $endpos (Ppat_tuple (List.rev $1)) }
| constr_longident pattern %prec prec_constr_appl
  { mkpat $startpos $endpos (Ppat_construct (mkrhs $startpos($1) $endpos($1) $1, Some $2, false)) }
| name_tag pattern %prec prec_constr_appl
  { mkpat $startpos $endpos (Ppat_variant ($1, Some $2)) }
| pattern COLONCOLON pattern
  { mkpat_cons (rloc $startpos($2) $endpos($2))
                 (ghpat $startpos $endpos (Ppat_tuple[$1;$3]))
                 (rloc $startpos $endpos) }
| LPAREN COLONCOLON RPAREN LPAREN pattern COMMA pattern RPAREN
  { mkpat_cons (rloc $startpos($2) $endpos($2))
                 (ghpat $startpos $endpos (Ppat_tuple[$5;$7]))
                 (rloc $startpos $endpos) }
| pattern BAR pattern
  { mkpat $startpos $endpos (Ppat_or ($1, $3)) }
| LAZY simple_pattern
  { mkpat $startpos $endpos (Ppat_lazy $2) }

simple_pattern:
| val_ident %prec below_EQUAL
  { mkpat $startpos $endpos (Ppat_var (mkrhs $startpos($1) $endpos($1) $1)) }
| UNDERSCORE
  { mkpat $startpos $endpos Ppat_any }
| signed_constant
  { mkpat $startpos $endpos (Ppat_constant $1) }
| CHAR DOTDOT CHAR
  { mkrangepat $startpos($1) $endpos($1) $1 $3 }
| constr_longident
  { mkpat $startpos $endpos (Ppat_construct (mkrhs $startpos($1) $endpos($1) $1, None, false)) }
| name_tag
  { mkpat $startpos $endpos (Ppat_variant ($1, None)) }
| SHARP type_longident
  { mkpat $startpos $endpos (Ppat_type (mkrhs $startpos($2) $endpos($2) $2)) }
| LBRACE lbl_pattern_list RBRACE
  { let (fields, closed) = $2 in mkpat $startpos $endpos (Ppat_record (fields, closed)) }
| LBRACKET pattern_semi_list option(SEMI) RBRACKET
  { reloc_pat $startpos $endpos (mktailpat $startpos $endpos (List.rev $2)) }
| LBRACKETBAR pattern_semi_list option(SEMI) BARRBRACKET
  { mkpat $startpos $endpos (Ppat_array (List.rev $2)) }
| LBRACKETBAR BARRBRACKET
  { mkpat $startpos $endpos (Ppat_array []) }
| LPAREN pattern RPAREN
  { reloc_pat $startpos $endpos $2 }
| LPAREN pattern COLON core_type RPAREN
  { mkpat $startpos $endpos (Ppat_constraint ($2, $4)) }
| LPAREN MODULE UIDENT RPAREN
  { mkpat $startpos $endpos (Ppat_unpack (mkrhs $startpos($3) $endpos($3) $3)) }
| LPAREN MODULE UIDENT COLON package_type RPAREN
  { mkpat $startpos $endpos (Ppat_constraint (mkpat $startpos $endpos
        (Ppat_unpack (mkrhs $startpos($3) $endpos($3) $3)),
        ghtyp $startpos($5) $endpos($5) (Ptyp_package $5))) }


pattern_comma_list:
| pattern_comma_list COMMA pattern
  { $3 :: $1 }
| pattern COMMA pattern
  { [$3; $1] }

pattern_semi_list:
| pattern
  { [$1] }
| pattern_semi_list SEMI pattern
  { $3 :: $1 }

lbl_pattern_list:
| lbl_pattern
  { $1, Closed }
| lbl_pattern SEMI
  { $1, Closed }
| lbl_pattern SEMI UNDERSCORE option(SEMI)
  { $1, Open }
| lbl_pattern SEMI lbl_pattern_list
  { let (fields, closed) = $3 in ($1 @ fields), closed }

lbl_pattern:
| label_longident EQUAL pattern
  { [mkrhs $startpos($1) $endpos($1) $1, $3] }
| label_longident
  { [mkrhs $startpos($1) $endpos($1) $1,
     pat_of_label $startpos($1) $endpos($1) $1] }


(* Primitive declarations *)

primitive_declaration:
| STRING
  { [$1] }
| STRING primitive_declaration
  { $1 :: $2 }


(* Type declarations *)

type_declarations:
| type_declaration
  { $1 }
| type_declarations AND type_declaration
  { $3 @ $1 }

type_declaration:
| optional_type_parameters LIDENT type_kind constraints
  { let (params, variance) = List.split $1 in
    let (kind, private_flag, manifest) = $3 in
    [mkrhs $startpos($2) $endpos($2) $2,
     { ptype_params = params;
       ptype_cstrs = List.rev $4;
       ptype_kind = kind;
       ptype_private = private_flag;
       ptype_manifest = manifest;
       ptype_variance = variance;
       ptype_loc = rloc $startpos $endpos
     }]
  }

constraints:
| (* empty *)
  { [] }
| constraints CONSTRAINT constrain
  { $3 @ $1 }

type_kind:
| (*empty*)
  { (Ptype_abstract, Public, None) }
| EQUAL core_type
  { (Ptype_abstract, Public, Some $2) }
| EQUAL PRIVATE core_type
  { (Ptype_abstract, Private, Some $3) }
| EQUAL constructor_declarations
  { regularize_type_kind (Ptype_variant (List.rev $2), Public, None) }
| EQUAL PRIVATE constructor_declarations
  { regularize_type_kind (Ptype_variant (List.rev $3), Private, None) }
| EQUAL private_flag BAR constructor_declarations
  { regularize_type_kind (Ptype_variant (List.rev $4), $2, None) }
| EQUAL private_flag LBRACE label_declarations option(SEMI) RBRACE
  { regularize_type_kind (Ptype_record (List.rev $4), $2, None) }
| EQUAL core_type EQUAL private_flag option(BAR) constructor_declarations
  { regularize_type_kind (Ptype_variant (List.rev $6), $4, Some $2) }
| EQUAL core_type EQUAL private_flag LBRACE label_declarations option(SEMI) RBRACE
  { regularize_type_kind (Ptype_record (List.rev $6), $4, Some $2) }

optional_type_parameters:
| (*empty*)
  { [] }
| optional_type_parameter
  { [$1] }
| LPAREN optional_type_parameter_list RPAREN
  { List.rev $2 }

optional_type_parameter:
| type_variance QUOTE ident
  { Some (mkrhs $startpos($3) $endpos($3) $3), $1 }
| type_variance UNDERSCORE
  { None, $1 }

optional_type_parameter_list:
| optional_type_parameter
  { [$1] }
| optional_type_parameter_list COMMA optional_type_parameter
  { $3 :: $1 }


type_parameters:
| (*empty*)
  { [] }
| type_parameter
  { $1 }
| LPAREN type_parameter_list RPAREN
  { List.rev $2 }

type_parameter:
| type_variance QUOTE ident
  { [mkrhs $startpos($3) $endpos($3) $3, $1] }

type_variance:
| (* empty *) { false, false }
| PLUS        { true, false }
| MINUS       { false, true }

type_parameter_list:
| type_parameter
  { $1 }
| type_parameter_list COMMA type_parameter
  { $3 @ $1 }

constructor_declarations:
| constructor_declaration
  { $1 }
| constructor_declarations BAR constructor_declaration
  { $3 @ $1 }

constructor_declaration:
| constr_ident generalized_constructor_arguments
  { let arg_types,ret_type = $2 in
    [mkrhs $startpos($1) $endpos($1) $1,
     arg_types, ret_type, rloc $startpos $endpos] }


constructor_arguments:
| (*empty*)
  { [] }
| OF core_type_list
  { List.rev $2 }

generalized_constructor_arguments:
| (*empty*)
  { ([],None) }
| OF core_type_list
  { (List.rev $2,None) }
| COLON core_type_list MINUSGREATER simple_core_type
  { (List.rev $2,Some $4) }
| COLON simple_core_type
  { ([],Some $2) }

label_declarations:
| label_declaration
  { $1 }
| label_declarations SEMI label_declaration
  { $3 @ $1 }

label_declaration:
| mutable_flag label COLON poly_type label_declaration_with
  { [mkrhs $startpos($2) $endpos($2) $2, $1, $4, rloc $startpos $endpos] }

label_declaration_with:
|           { () }
| WITH expr { () }


(* "with" constraints (additional type equations over signature components) *)

with_constraints:
| with_constraint
  { $1 }
| with_constraints AND with_constraint
  { $3 @ $1 }

with_constraint:
| TYPE type_parameters label_longident with_type_binder core_type constraints
  { let params, variance = List.split $2 in
      [(mkrhs $startpos($3) $endpos($3) $3,
       Pwith_type { ptype_params = List.map (fun x -> Some x) params;
                    ptype_cstrs = List.rev $6;
                    ptype_kind = Ptype_abstract;
                    ptype_manifest = Some $5;
                    ptype_private = $4;
                    ptype_variance = variance;
                    ptype_loc = rloc $startpos $endpos })] }
  (* used label_longident instead of type_longident to disallow
     functor applications in type path *)
| TYPE type_parameters label COLONEQUAL core_type
  { let params, variance = List.split $2 in
    [(mkrhs $startpos($3) $endpos($3) (Lident $3),
     Pwith_typesubst { ptype_params = List.map (fun x -> Some x) params;
                       ptype_cstrs = [];
                       ptype_kind = Ptype_abstract;
                       ptype_manifest = Some $5;
                       ptype_private = Public;
                       ptype_variance = variance;
                       ptype_loc = rloc $startpos $endpos })] }
| MODULE mod_longident EQUAL mod_ext_longident
  { [mkrhs $startpos($2) $endpos($2) $2, Pwith_module (mkrhs $startpos($4) $endpos($4) $4)] }
| MODULE UIDENT COLONEQUAL mod_ext_longident
  { [mkrhs $startpos($2) $endpos($2) (Lident $2), Pwith_modsubst (mkrhs $startpos($4) $endpos($4) $4)] }

with_type_binder:
| EQUAL          { Public }
| EQUAL PRIVATE  { Private }


(* Polymorphic types *)

typevar_list:
| QUOTE ident
  { [$2] }
| typevar_list QUOTE ident
  { $3 :: $1 }

poly_type:
| core_type
  { mktyp $startpos $endpos (Ptyp_poly ([], $1)) }
| typevar_list DOT core_type
  { mktyp $startpos $endpos (Ptyp_poly (List.rev $1, $3)) }


(* Core types *)

core_type:
| core_type2
  { $1 }
| core_type2 AS QUOTE ident
  { mktyp $startpos $endpos (Ptyp_alias ($1, $4)) }

core_type2:
| simple_core_type_or_tuple
  { $1 }
| QUESTION LIDENT COLON core_type2 MINUSGREATER core_type2
  { mktyp $startpos $endpos (Ptyp_arrow ("?" ^ $2 , mkoption  $4, $6)) }
| OPTLABEL core_type2 MINUSGREATER core_type2
  { mktyp $startpos $endpos (Ptyp_arrow ("?" ^ $1 , mkoption  $2, $4)) }
| LIDENT COLON core_type2 MINUSGREATER core_type2
  { mktyp $startpos $endpos (Ptyp_arrow ($1, $3, $5)) }
| core_type2 MINUSGREATER core_type2
  { mktyp $startpos $endpos (Ptyp_arrow ("", $1, $3)) }


simple_core_type:
| simple_core_type2
  { $1 }
| LPAREN core_type_comma_list RPAREN
  { match $2 with [sty] -> sty | _ -> raise Parsing.Parse_error }

simple_core_type2:
| QUOTE ident
  { mktyp $startpos $endpos (Ptyp_var $2) }
| UNDERSCORE
  { mktyp $startpos $endpos Ptyp_any }
| type_longident
  { mktyp $startpos $endpos (Ptyp_constr (mkrhs $startpos($1) $endpos($1) $1, [])) }
| simple_core_type2 type_longident
  { mktyp $startpos $endpos (Ptyp_constr (mkrhs $startpos($2) $endpos($2) $2, [$1])) }
| LPAREN core_type_comma_list RPAREN type_longident
  { mktyp $startpos $endpos (Ptyp_constr (mkrhs $startpos($4) $endpos($4) $4, List.rev $2)) }
| LESS meth_list GREATER
  { mktyp $startpos $endpos (Ptyp_object $2) }
| LESS GREATER
  { mktyp $startpos $endpos (Ptyp_object []) }
| SHARP class_longident opt_present
  { mktyp $startpos $endpos (Ptyp_class (mkrhs $startpos($2) $endpos($2) $2, [], $3)) }
| simple_core_type2 SHARP class_longident opt_present
  { mktyp $startpos $endpos (Ptyp_class (mkrhs $startpos($3) $endpos($3) $3, [$1], $4)) }
| LPAREN core_type_comma_list RPAREN SHARP class_longident opt_present
  { mktyp $startpos $endpos (Ptyp_class (mkrhs $startpos($5) $endpos($5) $5, List.rev $2, $6)) }
| LBRACKET tag_field RBRACKET
  { mktyp $startpos $endpos (Ptyp_variant ($2, true, None)) }
| LBRACKET BAR row_field_list RBRACKET
  { mktyp $startpos $endpos (Ptyp_variant (List.rev $3, true, None)) }
| LBRACKET row_field BAR row_field_list RBRACKET
  { mktyp $startpos $endpos (Ptyp_variant ($2 @ List.rev $4, true, None)) }
| LBRACKETGREATER option(BAR) row_field_list RBRACKET
  { mktyp $startpos $endpos (Ptyp_variant (List.rev $3, false, None)) }
| LBRACKETGREATER RBRACKET
  { mktyp $startpos $endpos (Ptyp_variant ([], false, None)) }
| LBRACKETLESS option(BAR) row_field_list RBRACKET
  { mktyp $startpos $endpos (Ptyp_variant (List.rev $3, true, Some [])) }
| LBRACKETLESS option(BAR) row_field_list GREATER name_tag_list RBRACKET
  { mktyp $startpos $endpos (Ptyp_variant (List.rev $3, true, Some (List.rev $5))) }
| LPAREN MODULE package_type RPAREN
  { mktyp $startpos $endpos (Ptyp_package $3) }

package_type:
| mty_longident
  { (mkrhs $startpos($1) $endpos($1) $1, []) }
| mty_longident WITH package_type_cstrs
  { (mkrhs $startpos($1) $endpos($1) $1, $3) }

package_type_cstr:
| TYPE label_longident EQUAL core_type
  { [mkrhs $startpos($2) $endpos($2) $2, $4] }

package_type_cstrs:
| package_type_cstr
  { $1 }
| package_type_cstr AND package_type_cstrs
  { $1 @ $3 }

row_field_list:
| row_field
  { $1 }
| row_field_list BAR row_field
  { $3 @ $1 }

row_field:
| tag_field
  { $1 }
| simple_core_type
  { [Rinherit $1] }

tag_field:
| name_tag OF opt_ampersand amper_type_list
  { [Rtag ($1, $3, List.rev $4)] }
| name_tag
  { [Rtag ($1, true, [])] }

opt_ampersand:
| AMPERSAND                                   { true }
| (* empty *)                                 { false }

amper_type_list:
    core_type                                   { [$1] }
  | amper_type_list AMPERSAND core_type         { $3 :: $1 }

opt_present:
| LBRACKETGREATER name_tag_list RBRACKET
  { List.rev $2 }
| (* empty *)
  { [] }

name_tag_list:
| name_tag
  { [$1] }
| name_tag_list name_tag
  { $2 :: $1 }

simple_core_type_or_tuple:
| simple_core_type
  { $1 }
| simple_core_type STAR core_type_list
  { mktyp $startpos $endpos (Ptyp_tuple ($1 :: List.rev $3)) }

core_type_comma_list:
| core_type                                   { [$1] }
| core_type_comma_list COMMA core_type        { $3 :: $1 }

core_type_list:
| simple_core_type                            { [$1] }
| core_type_list STAR simple_core_type        { $3 :: $1 }

meth_list:
| field SEMI meth_list
  { $1 @ $3 }
| field option(SEMI)
  { $1 }
| DOTDOT
  { [mkfield $startpos $endpos Pfield_var] }

field:
| label COLON poly_type
  { [mkfield $startpos $endpos (Pfield ($1, $3))] }

label:
| LIDENT { $1 }


(* Constants *)

constant:
| INT       { Const_int $1 }
| CHAR      { Const_char $1 }
| STRING    { Const_string $1 }
| FLOAT     { Const_float $1 }
| INT32     { Const_int32 $1 }
| INT64     { Const_int64 $1 }
| NATIVEINT { Const_nativeint $1 }

signed_constant:
| constant        { $1 }
| MINUS INT       { Const_int (- $2) }
| MINUS FLOAT     { Const_float ("-" ^ $2) }
| MINUS INT32     { Const_int32 (Int32.neg $2) }
| MINUS INT64     { Const_int64 (Int64.neg $2) }
| MINUS NATIVEINT { Const_nativeint (Nativeint.neg $2) }
| PLUS INT        { Const_int $2 }
| PLUS FLOAT      { Const_float $2 }
| PLUS INT32      { Const_int32 $2 }
| PLUS INT64      { Const_int64 $2 }
| PLUS NATIVEINT  { Const_nativeint $2 }

(* Identifiers and long identifiers *)

ident:
| UIDENT { $1 }
| LIDENT { $1 }

val_ident:
| LIDENT                 { $1 }
| LPAREN operator RPAREN { $2 }

operator:
| PREFIXOP   { $1 }
| INFIXOP0   { $1 }
| INFIXOP1   { $1 }
| INFIXOP2   { $1 }
| INFIXOP3   { $1 }
| INFIXOP4   { $1 }
| BANG       { "!" }
| PLUS       { "+" }
| PLUSDOT    { "+." }
| MINUS      { "-" }
| MINUSDOT   { "-." }
| STAR       { "*" }
| EQUAL      { "=" }
| LESS       { "<" }
| GREATER    { ">" }
| OR         { "or" }
| BARBAR     { "||" }
| AMPERSAND  { "&" }
| AMPERAMPER { "&&" }
| COLONEQUAL { ":=" }

constr_ident:
| UIDENT        { $1 }
| LPAREN RPAREN { "()" }
| COLONCOLON    { "::" }
| FALSE         { "false" }
| TRUE          { "true" }

val_longident:
| val_ident
  { Lident $1 }
| mod_longident DOT val_ident
  { Ldot ($1, $3) }

constr_longident:
| mod_longident %prec below_DOT { $1 }
| LBRACKET RBRACKET { Lident "[]" }
| LPAREN RPAREN     { Lident "()" }
| FALSE             { Lident "false" }
| TRUE              { Lident "true" }

label_longident:
  LIDENT                   { Lident $1 }
| mod_longident DOT LIDENT { Ldot ($1, $3) }

type_longident:
  LIDENT                       { Lident $1 }
| mod_ext_longident DOT LIDENT { Ldot ($1, $3) }

mod_longident:
  UIDENT                   { Lident $1 }
| mod_longident DOT UIDENT { Ldot ($1, $3) }

mod_ext_longident:
  UIDENT                       { Lident $1 }
| mod_ext_longident DOT UIDENT { Ldot ($1, $3) }
| mod_ext_longident LPAREN mod_ext_longident RPAREN
  { lapply $startpos $endpos $1 $3 }

mty_longident:
  ident                       { Lident $1 }
| mod_ext_longident DOT ident { Ldot ($1, $3) }

clty_longident:
  LIDENT                       { Lident $1 }
| mod_ext_longident DOT LIDENT { Ldot ($1, $3) }

class_longident:
| LIDENT                   { Lident $1 }
| mod_longident DOT LIDENT { Ldot ($1, $3) }

(* Introduced for merlin, to make stack structure more explicit *)

mod_open:
| override_flag mod_longident { ($1, mkrhs $startpos($2) $endpos($2) $2) }

new_type:
| LPAREN TYPE LIDENT RPAREN { $3 }

constrained_seq_expr:
| e = seq_expr t = type_constraint
  { check_constraint (mkexp $startpos $endpos) t e }

(* Miscellaneous *)

name_tag:
| BACKQUOTE ident { $2 }

rec_flag:
| (* empty *)  { Nonrecursive }
| REC          { Recursive }

direction_flag:
| TO           { Upto }
| DOWNTO       { Downto }

private_flag:
| (* empty *)  { Public }
| PRIVATE      { Private }

mutable_flag:
| (* empty *)  { Immutable }
| MUTABLE      { Mutable }

virtual_flag:
| (* empty *)  { Concrete }
| VIRTUAL      { Virtual }

override_flag:
| (* empty *)  { Fresh }
| BANG         { Override }

subtractive:
| MINUS        { "-" }
| MINUSDOT     { "-." }

additive:
| PLUS         { "+" }
| PLUSDOT      { "+." }

%%
