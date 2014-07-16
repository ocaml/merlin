%{
open Asttypes
open Longident
open Parsetree
open Ast_helper


let rloc loc_start loc_end =
  { Location. loc_start; loc_end; loc_ghost = false; }
let gloc loc_start loc_end =
  { Location. loc_start; loc_end; loc_ghost = true; }
let mkloc =
  Location.mkloc

let mktyp startpos endpos d   = Typ.mk ~loc:(rloc startpos endpos) d
let mkpat startpos endpos d   = Pat.mk ~loc:(rloc startpos endpos) d
let mkexp startpos endpos d   = Exp.mk ~loc:(rloc startpos endpos) d
let mkmty startpos endpos d   = Mty.mk ~loc:(rloc startpos endpos) d
let mksig startpos endpos d   = [Sig.mk ~loc:(rloc startpos endpos) d]
let mkmod startpos endpos d   = Mod.mk ~loc:(rloc startpos endpos) d
let mkstr startpos endpos d   = [Str.mk ~loc:(rloc startpos endpos) d]
let ghstr startpos endpos d   = [Str.mk ~loc:(gloc startpos endpos) d]
let mkclass startpos endpos d = Cl.mk  ~loc:(rloc startpos endpos) d
let mkcty startpos endpos d   = Cty.mk ~loc:(rloc startpos endpos) d
let mkctf startpos endpos ?attrs d = Ctf.mk ~loc:(rloc startpos endpos) ?attrs d
let mkcf  startpos endpos ?attrs d = [Cf.mk  ~loc:(rloc startpos endpos) ?attrs d]

let mkrhs startpos endpos rhs = mkloc rhs (rloc startpos endpos)
let mkoption d =
  let loc = {d.ptyp_loc with Location. loc_ghost = true} in
  Typ.mk ~loc (Ptyp_constr(mkloc (Ldot (Lident "*predef*", "option")) loc,[d]))

let reloc_pat startpos endpos x= { x with ppat_loc = rloc startpos endpos };;
let reloc_exp startpos endpos x= { x with pexp_loc = rloc startpos endpos };;
let reloc_exp_fake startpos endpos x =
  { x with pexp_loc =
      Parsing_aux.pack_fake_location x.pexp_loc ~fake:(rloc startpos endpos) }

let mkoperator startpos endpos name =
  let loc = rloc startpos endpos in
  Exp.mk ~loc (Pexp_ident(mkloc (Lident name) loc))

let mkpatvar startpos endpos name =
  Pat.mk ~loc:(rloc startpos endpos) (Ppat_var (mkrhs startpos endpos name))

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
let ghexp startpos endpos d = Exp.mk ~loc:(gloc startpos endpos) d
let ghpat startpos endpos d = Pat.mk ~loc:(gloc startpos endpos) d
let ghtyp startpos endpos d = Typ.mk ~loc:(gloc startpos endpos) d
let ghloc startpos endpos d = { txt = d; loc = gloc startpos endpos }

let mkinfix startpos endpos arg1 startpos2 endpos2 name arg2 =
  mkexp startpos endpos
    (Pexp_apply(mkoperator startpos2 endpos2 name, ["", arg1; "", arg2]))

let neg_float_string f =
  if String.length f > 0 && f.[0] = '-'
  then String.sub f 1 (String.length f - 1)
  else "-" ^ f

let mkuminus startpos endpos name arg =
  match name, arg.pexp_desc with
  | "-", Pexp_constant(Const_int n) ->
      mkexp startpos endpos (Pexp_constant(Const_int(-n)))
  | "-", Pexp_constant(Const_int32 n) ->
      mkexp startpos endpos (Pexp_constant(Const_int32(Int32.neg n)))
  | "-", Pexp_constant(Const_int64 n) ->
      mkexp startpos endpos (Pexp_constant(Const_int64(Int64.neg n)))
  | "-", Pexp_constant(Const_nativeint n) ->
      mkexp startpos endpos (Pexp_constant(Const_nativeint(Nativeint.neg n)))
  | ("-" | "-."), Pexp_constant(Const_float f) ->
      mkexp startpos endpos (Pexp_constant(Const_float(neg_float_string f)))
  | _ ->
      mkexp startpos endpos (Pexp_apply(mkoperator startpos endpos ("~" ^ name), ["", arg]))

let mkuplus startpos endpos name arg =
  let desc = arg.pexp_desc in
  match name, desc with
  | "+", Pexp_constant(Const_int _)
  | "+", Pexp_constant(Const_int32 _)
  | "+", Pexp_constant(Const_int64 _)
  | "+", Pexp_constant(Const_nativeint _)
  | ("+" | "+."), Pexp_constant(Const_float _) -> mkexp startpos endpos desc
  | _ ->
      mkexp startpos endpos (Pexp_apply(mkoperator startpos endpos ("~" ^ name), ["", arg]))

let mkexp_cons consloc args loc =
  Exp.mk ~loc (Pexp_construct(mkloc (Lident "::") consloc, Some args))

let mkpat_cons consloc args loc =
  Pat.mk ~loc (Ppat_construct(mkloc (Lident "::") consloc, Some args))

let rec mktailexp startpos endpos = function
    [] ->
      let loc = gloc startpos endpos in
      let nil = { txt = Lident "[]"; loc = loc } in
      Exp.mk ~loc (Pexp_construct (nil, None))
  | e1 :: el ->
      let open Location in
      let exp_el = mktailexp e1.pexp_loc.loc_end endpos el in
      let loc = gloc e1.pexp_loc.loc_start exp_el.pexp_loc.loc_end in
      let arg = Exp.mk ~loc (Pexp_tuple [e1; exp_el]) in
      mkexp_cons loc arg loc

let rec mktailpat startpos endpos = function
    [] ->
      let loc = gloc startpos endpos in
      let nil = { txt = Lident "[]"; loc = loc } in
      Pat.mk ~loc (Ppat_construct (nil, None))
  | p1 :: pl ->
      let open Location in
      let pat_pl = mktailpat p1.ppat_loc.loc_end endpos pl in
      let loc = gloc p1.ppat_loc.loc_start pat_pl.ppat_loc.loc_end in
      let arg = Pat.mk ~loc (Ppat_tuple [p1; pat_pl]) in
      mkpat_cons loc arg loc

let mkstrexp e attrs =
  { pstr_desc = Pstr_eval (e, attrs); pstr_loc = e.pexp_loc }

let mkexp_constraint startpos endpos e (t1, t2) =
  match t1, t2 with
  | Some t, None -> ghexp startpos endpos (Pexp_constraint(e, t))
  | _, Some t -> ghexp startpos endpos (Pexp_coerce(e, t1, t))
  | None, None -> e

let array_function startpos endpos str name =
  ghloc startpos endpos
    (Ldot(Lident str, (if Clflags.fast () then "unsafe_" ^ name else name)))

let syntax_error startpos endpos =
  Parsing_aux.raise_warning (Syntaxerr.Escape_error (rloc startpos endpos))

let unclosed opening_name opstart opend closing_name clstart clend =
  raise
    Syntaxerr.(Error (Unclosed (rloc opstart opend, opening_name,
                                rloc clstart clend, closing_name)))

let expecting startpos endpos nonterm =
  raise
    Syntaxerr.(Error (Expecting (rloc startpos endpos, nonterm)))

let not_expecting startpos endpos nonterm =
  Parsing_aux.raise_warning
    Syntaxerr.(Error (Not_expecting (rloc startpos endpos, nonterm)))

let bigarray_function startpos endpos str name =
  ghloc startpos endpos (Ldot(Ldot(Lident "Bigarray", str), name))

let bigarray_untuplify = function
    { pexp_desc = Pexp_tuple explist; pexp_loc = _ } -> explist
  | exp -> [exp]

let bigarray_get (startpos,endpos) (startop,endop) arr arg =
  let get = if Clflags.fast () then "unsafe_get" else "get" in
  let ghexp = ghexp startop endop in
  let mkexp = mkexp startpos endpos in
  let bigarray_function = bigarray_function startop endop in
  match bigarray_untuplify arg with
    [c1] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array1" get)),
                       ["", arr; "", c1]))
  | [c1;c2] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array2" get)),
                       ["", arr; "", c1; "", c2]))
  | [c1;c2;c3] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array3" get)),
                       ["", arr; "", c1; "", c2; "", c3]))
  | coords ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Genarray" "get")),
                       ["", arr; "", ghexp(Pexp_array coords)]))

let bigarray_set (startpos,endpos) (startop,endop) arr arg newval =
  let set = if Clflags.fast () then "unsafe_set" else "set" in
  let ghexp = ghexp startop endop in
  let mkexp = mkexp startpos endpos in
  let bigarray_function = bigarray_function startop endop in
  match bigarray_untuplify arg with
    [c1] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array1" set)),
                       ["", arr; "", c1; "", newval]))
  | [c1;c2] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array2" set)),
                       ["", arr; "", c1; "", c2; "", newval]))
  | [c1;c2;c3] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array3" set)),
                       ["", arr; "", c1; "", c2; "", c3; "", newval]))
  | coords ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Genarray" "set")),
                       ["", arr;
                        "", ghexp(Pexp_array coords);
                        "", newval]))

let lapply startpos endpos p1 p2 =
  if Clflags.applicative_functors ()
  then Lapply(p1, p2)
  else raise (Syntaxerr.Error(Syntaxerr.Applicative_path (rloc startpos endpos)))

let exp_of_label startpos endpos lbl =
  mkexp startpos endpos (Pexp_ident(mkrhs startpos endpos (Lident(Longident.last lbl))))

let pat_of_label startpos endpos lbl =
  mkpat startpos endpos (Ppat_var (mkrhs startpos endpos (Longident.last lbl)))

let check_variable vl loc v =
  if List.mem v vl then
    Parsing_aux.raise_warning Syntaxerr.(Error(Variable_in_scope(loc,v)))

let varify_constructors var_names t =
  let rec loop t =
    let desc =
      match t.ptyp_desc with
      | Ptyp_any -> Ptyp_any
      | Ptyp_var x ->
          check_variable var_names t.ptyp_loc x;
          Ptyp_var x
      | Ptyp_arrow (label,core_type,core_type') ->
          Ptyp_arrow(label, loop core_type, loop core_type')
      | Ptyp_tuple lst -> Ptyp_tuple (List.map loop lst)
      | Ptyp_constr( { txt = Lident s }, []) when List.mem s var_names ->
          Ptyp_var s
      | Ptyp_constr(longident, lst) ->
          Ptyp_constr(longident, List.map loop lst)
      | Ptyp_object (lst, o) ->
          Ptyp_object (List.map (fun (s, attrs, t) -> (s, attrs, loop t)) lst, o)
      | Ptyp_class (longident, lst) ->
          Ptyp_class (longident, List.map loop lst)
      | Ptyp_alias(core_type, string) ->
          check_variable var_names t.ptyp_loc string;
          Ptyp_alias(loop core_type, string)
      | Ptyp_variant(row_field_list, flag, lbl_lst_option) ->
          Ptyp_variant(List.map loop_row_field row_field_list,
                       flag, lbl_lst_option)
      | Ptyp_poly(string_lst, core_type) ->
          List.iter (check_variable var_names t.ptyp_loc) string_lst;
          Ptyp_poly(string_lst, loop core_type)
      | Ptyp_package(longident,lst) ->
          Ptyp_package(longident,List.map (fun (n,typ) -> (n,loop typ) ) lst)
      | Ptyp_extension (s, arg) ->
          Ptyp_extension (s, arg)
    in
    {t with ptyp_desc = desc}
  and loop_row_field  =
    function
      | Rtag(label,attrs,flag,lst) ->
          Rtag(label,attrs,flag,List.map loop lst)
      | Rinherit t ->
          Rinherit (loop t)
  in
  loop t

let wrap_type_annotation startpos endpos newtypes core_type body =
  let mkexp = mkexp startpos endpos in
  let ghtyp = ghtyp startpos endpos in
  let exp = mkexp(Pexp_constraint(body,core_type)) in
  let exp =
    List.fold_right (fun newtype exp -> mkexp (Pexp_newtype (newtype, exp)))
      newtypes exp
  in
  (exp, ghtyp (Ptyp_poly(newtypes,varify_constructors newtypes core_type)))

let wrap_exp_attrs startpos endpos body (ext, attrs) =
  (* todo: keep exact location for the entire attribute *)
  let body = {body with pexp_attributes = attrs @ body.pexp_attributes} in
  match ext with
  | None -> body
  | Some id -> ghexp startpos endpos (Pexp_extension (id, PStr [mkstrexp body []]))

let mkexp_attrs startpos endpos d attrs =
  wrap_exp_attrs startpos endpos (mkexp startpos endpos d) attrs

let fake_tydecl tydecl = tydecl.ptype_name, tydecl
let fake_untydecl (ptype_name,tydecl) = {tydecl with ptype_name}
let tag_nonrec (id, a) = fake_untydecl(Fake.Nonrec.add id, a)
let fake_vb_app f vb = {vb with pvb_expr = Fake.app f vb.pvb_expr}

%}

(**
 * `Shift n             : when at the left of the annotated symbol, it is a
 *                        valid recovery to shift it, and this strategy has
 *                        priority [n]
 * `Shift_token (n,tok) : when at the right of the annotated symbol shifting
 *                        [tok] is a strategy with priority [n]
 * `Cost n              : the annotated symbol cost is adjusted by [n]
 * `Indent n            : the annotated symbol column should be adjusted by
 *                        [n] before computing recovery guide
 * `Unclosed name       : the current symbol is the opening a pair that should
 *                        closed. [name] will be reported to the user in case
 *                        of syntax error.
 * `Close               : this symbol close the pair opened by an earlier
 *                        `Unclosed
 * `Item kind           : this symbol begins the definition of an [kind] item,
 *                        where kind is let, type, ...
 **)
%annot <[ `Shift of int | `Shift_token of int * token | `Cost of int
        | `Indent of int
        | `Unclosed of string | `Close
        | `Item of string ]>

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
%token LBRACKETAT
%token LBRACKETATAT
%token LBRACKETATATAT
%token LBRACKETBAR
%token LBRACKETGREATER
%token LBRACKETLESS
%token LBRACKETPERCENT
%token LBRACKETPERCENTPERCENT
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
%token PERCENT
%token PLUS
%token PLUSDOT
%token PLUSEQ
%token <string> PREFIXOP
%token PRIVATE
%token QUESTION
%token QUOTE
%token RBRACE
%token RBRACKET
%token REC
%token RPAREN
%token SEMI
%token SEMISEMI
%token SHARP
%token SIG
%token STAR
%token <string * string option> STRING
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

%token EOL

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
%token NONREC

%token ENTRYPOINT EXITPOINT

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
%nonassoc below_LBRACKETAT
%nonassoc LBRACKETAT
%nonassoc LBRACKETATAT
%right    COLONCOLON                    (* expr (e :: e :: e) *)
%left     INFIXOP2 PLUS PLUSDOT MINUS MINUSDOT PLUSEQ (* expr (e OP e OP e) *)
%left     INFIXOP3 STAR PERCENT         (* expr (e OP e OP e) *)
%right    INFIXOP4                      (* expr (e OP e OP e) *)
%nonassoc prec_unary_minus prec_unary_plus (* unary - *)
%nonassoc prec_constant_constructor     (* cf. simple_expr (C versus C x) *)
%nonassoc prec_constr_appl              (* above AS BAR COLONCOLON COMMA *)
%nonassoc below_SHARP
%nonassoc SHARP                         (* simple_expr/toplevel_directive *)
%nonassoc below_DOT
%nonassoc DOT

(* Finally, the first tokens of simple_expr are above everything else. *)
%nonassoc BACKQUOTE BANG BEGIN CHAR FALSE FLOAT INT INT32 INT64
          LBRACE LBRACELESS LBRACKET LBRACKETBAR LIDENT LPAREN
          NEW NATIVEINT PREFIXOP STRING TRUE UIDENT
          LBRACKETPERCENT LBRACKETPERCENTPERCENT
          P4_QUOTATION JSNEW

(* Entry points *)

(* for implementation files *)
%start implementation
%type <Parsetree.structure> implementation

(* for interface files *)
%start interface
%type <Parsetree.signature> interface

(* merlin: for inline expression *)
%start parse_expression
%type <Parsetree.expression> parse_expression

(* Prevent some warnings... *)
%start dummy
%type <unit> dummy

%%

(* Entry points *)

implementation:
| ENTRYPOINT v1 = structure EOF
    { v1 }

interface:
| ENTRYPOINT v1 = signature EOF
    { v1 }

parse_expression:
| ENTRYPOINT v1 = seq_expr EOF
    { v1 }

dummy:
| EOL
| NONREC
| COMMENT
| GREATERRBRACKET
| ENTRYPOINT
| LET_LWT
| TRY_LWT
| MATCH_LWT
| FINALLY_LWT
| FOR_LWT
| WHILE_LWT
| JSNEW
| P4_QUOTATION
| OUNIT_TEST
| OUNIT_TEST_UNIT
| OUNIT_TEST_MODULE
| OUNIT_BENCH
| OUNIT_BENCH_FUN
| OUNIT_BENCH_INDEXED
| OUNIT_BENCH_MODULE
    { () }

(* Module expressions *)

functor_arg:
| LPAREN _2 = RPAREN
    { mkrhs $startpos(_2) $endpos(_2) "()", None }
| LPAREN v2 = functor_arg_name COLON v4 = module_type RPAREN
    { mkrhs $startpos(v2) $endpos(v2) v2, Some v4 }

functor_arg_name:
| v1 = UIDENT
    { v1 }
| UNDERSCORE
    { "_" }

functor_args:
| v1 = functor_args v2 = functor_arg
    { v2 :: v1 }
| v1 = functor_arg
    { [ v1 ] }

module_expr:
| v1 = mod_longident
    { mkmod $startpos $endpos (Pmod_ident (mkrhs $startpos(v1) $endpos(v1) v1)) }
| STRUCT @{`Unclosed "struct"}
  v2 = structure END
    { mkmod $startpos $endpos (Pmod_structure(v2)) }
| FUNCTOR v2 = functor_args MINUSGREATER v4 = module_expr
    { List.fold_left (fun acc (n, t) -> mkmod $startpos $endpos (Pmod_functor(n, t, acc))) v4 v2 }
| v1 = module_expr LPAREN v3 = module_expr RPAREN
    { mkmod $startpos $endpos (Pmod_apply(v1, v3)) }
| v1 = module_expr LPAREN RPAREN
    { mkmod $startpos $endpos (Pmod_apply(v1, mkmod $startpos $endpos (Pmod_structure []))) }
| LPAREN @{`Unclosed "("}
  v2 = module_expr COLON v4 = module_type RPAREN
    { mkmod $startpos $endpos (Pmod_constraint(v2, v4)) }
| LPAREN @{`Unclosed "("} v2 = module_expr RPAREN
    { v2 }
| LPAREN @{`Unclosed "("}
  VAL v3 = expr RPAREN
    { mkmod $startpos $endpos (Pmod_unpack v3) }
| LPAREN @{`Unclosed "("}
  VAL v3 = expr COLON v5 = package_type RPAREN
    { mkmod $startpos $endpos (Pmod_unpack(
              ghexp $startpos $endpos (Pexp_constraint(v3, ghtyp $startpos $endpos (Ptyp_package v5))))) }
| LPAREN @{`Unclosed "("}
  VAL v3 = expr COLON v5 = package_type COLONGREATER v7 = package_type RPAREN
    { mkmod $startpos $endpos (Pmod_unpack(
              ghexp $startpos $endpos (Pexp_coerce(v3, Some(ghtyp $startpos $endpos (Ptyp_package v5)),
                                    ghtyp $startpos $endpos (Ptyp_package v7))))) }
| LPAREN @{`Unclosed "("}
  VAL v3 = expr COLONGREATER v5 = package_type RPAREN
    { mkmod $startpos $endpos (Pmod_unpack(
              ghexp $startpos $endpos (Pexp_coerce(v3, None, ghtyp $startpos $endpos (Ptyp_package v5))))) }
| v1 = module_expr v2 = attribute
    { Mod.attr v1 v2 }
| v1 = extension
    { mkmod $startpos $endpos (Pmod_extension v1) }

structure:
| v = structure_head
| v = structure_head EXITPOINT
  { v }

structure_head:
| v1 = seq_expr v2 = post_item_attributes v3 = structure_tail
  @{`Shift_token (1,EXITPOINT)}
    { mkstrexp v1 v2 :: v3 }
| v1 = structure_tail
  @{`Shift_token (1,EXITPOINT)}
    { v1 }

structure_tail:
| (* empty *)
    { [] }
| SEMISEMI v2 = structure_head
  @{`Shift_token (1,EXITPOINT)}
    { v2 }
| v1 = structure_item v2 = structure_tail
  @{`Shift_token (1,EXITPOINT)}
    { v1 @ v2 }

structure_item:
| LET @{`Item "let"}
  v2 = ext_attributes v3 = rec_flag v4 = let_bindings
    {
      match v4 with
        [ {pvb_pat = { ppat_desc = Ppat_any; ppat_loc = _ };
           pvb_expr = exp; pvb_attributes = attrs}] ->
          let exp = wrap_exp_attrs $startpos $endpos exp v2 in
          mkstr $startpos $endpos (Pstr_eval (exp, attrs))
      | l ->
        let str = mkstr $startpos $endpos (Pstr_value(v3, List.rev l)) in
        let (ext, attrs) = v2 in
        if attrs <> [] then not_expecting $startpos(v2) $endpos(v2) "attribute";
        match ext with
        | None -> str
        | Some id -> ghstr $startpos $endpos (Pstr_extension((id, PStr str), []))
    }
| EXTERNAL @{`Item "external"}
  v2 = val_ident COLON v4 = core_type EQUAL v6 = primitive_declaration
  v7 = post_item_attributes
    { mkstr $startpos $endpos
        (Pstr_primitive (Val.mk (mkrhs $startpos(v2) $endpos(v2) v2) v4
                           ~prim:v6 ~attrs:v7 ~loc:(rloc $startpos $endpos))) }
| TYPE @{`Item "type"}
  decls = type_declarations
    { mkstr $startpos $endpos (Pstr_type (List.rev decls) ) }
| TYPE  @{`Item "type"}
  v2 = str_type_extension
    { mkstr $startpos $endpos (Pstr_typext v2) }
| EXCEPTION @{`Item "exception"}
  v2 = str_exception_declaration
    { mkstr $startpos $endpos (Pstr_exception v2) }
| MODULE @{`Item "module"}
  v2 = module_binding
    { mkstr $startpos $endpos (Pstr_module v2) }
| MODULE REC @{`Item "recursive module"}
  v3 = module_bindings
    { mkstr $startpos $endpos (Pstr_recmodule(List.rev v3)) }
| MODULE TYPE @{`Item "module type"}
  v3 = ident v4 = post_item_attributes
    { mkstr $startpos $endpos (Pstr_modtype (Mtd.mk (mkrhs $startpos(v3) $endpos(v3) v3)
                              ~attrs:v4 ~loc:(rloc $startpos $endpos))) }
| MODULE TYPE @{`Item "module type"}
  v3 = ident EQUAL v5 = module_type v6 = post_item_attributes
    { mkstr $startpos $endpos (Pstr_modtype (Mtd.mk (mkrhs $startpos(v3) $endpos(v3) v3)
                              ~typ:v5 ~attrs:v6 ~loc:(rloc $startpos $endpos))) }
| v1 = open_statement
    { mkstr $startpos $endpos (Pstr_open v1) }
| CLASS @{`Item "class"}
  v2 = class_declarations
    { mkstr $startpos $endpos (Pstr_class (List.rev v2)) }
| CLASS TYPE @{`Item "class type"}
  v3 = class_type_declarations
    { mkstr $startpos $endpos (Pstr_class_type (List.rev v3)) }
| INCLUDE @{`Item "include"}
  v2 = module_expr v3 = post_item_attributes
    { mkstr $startpos $endpos (Pstr_include (Incl.mk v2 ~attrs:v3
                                             ~loc:(rloc $startpos $endpos))) }
| v1 = item_extension v2 = post_item_attributes
    { mkstr $startpos $endpos (Pstr_extension (v1, v2)) }
| v1 = floating_attribute
    { mkstr $startpos $endpos (Pstr_attribute v1) }

module_binding_body:
| EQUAL v2 = module_expr
    { v2 }
| COLON v2 = module_type EQUAL v4 = module_expr
    { mkmod $startpos $endpos (Pmod_constraint(v4, v2)) }
| v1 = functor_arg v2 = module_binding_body
    { mkmod $startpos $endpos (Pmod_functor(fst v1, snd v1, v2)) }

module_bindings:
| v1 = module_binding
    { [v1] }
| v1 = module_bindings AND v3 = module_binding
    { v3 :: v1 }

module_binding:
| v1 = UIDENT v2 = module_binding_body v3 = post_item_attributes
    { Mb.mk (mkrhs $startpos(v1) $endpos(v1) v1) v2 ~attrs:v3 ~loc:(rloc $startpos $endpos) }

(* Module types *)

module_type:
| v1 = mty_longident
    { mkmty $startpos $endpos (Pmty_ident (mkrhs $startpos(v1) $endpos(v1) v1)) }
| SIG @{`Unclosed "sig"} v2 = signature END
    { mkmty $startpos $endpos (Pmty_signature v2) }
| FUNCTOR v2 = functor_args MINUSGREATER v4 = module_type %prec below_WITH
    { List.fold_left (fun acc (n, t) -> mkmty $startpos $endpos (Pmty_functor(n, t, acc))) v4 v2 }
| v1 = module_type WITH v3 = with_constraints
    { mkmty $startpos $endpos (Pmty_with(v1, List.rev v3)) }
| MODULE TYPE OF v4 = module_expr %prec below_LBRACKETAT
    { mkmty $startpos $endpos (Pmty_typeof v4) }
| LPAREN @{`Unclosed "("} MODULE v3 = mod_longident RPAREN
    { mkmty $startpos $endpos  (Pmty_alias (mkrhs $startpos(v3) $endpos(v3) v3)) }
| LPAREN @{`Unclosed "("} v2 = module_type RPAREN
    { v2 }
| v1 = extension
    { mkmty $startpos $endpos (Pmty_extension v1) }
| v1 = module_type v2 = attribute
    { Mty.attr v1 v2 }

signature:
| (* empty *)
    { [] }
| SEMISEMI v2 = signature
    { v2 }
| v1 = signature_item v2 = signature
    { v1 @ v2 }

signature_item:
| VAL @{`Item "val"}
  v2 = val_ident COLON v4 = core_type v5 = post_item_attributes
    { mksig $startpos $endpos (Psig_value
                (Val.mk (mkrhs $startpos(v2) $endpos(v2) v2) v4 ~attrs:v5 ~loc:(rloc $startpos $endpos))) }
| EXTERNAL @{`Item "external"}
  v2 = val_ident COLON v4 = core_type EQUAL v6 = primitive_declaration v7 = post_item_attributes
    { mksig $startpos $endpos (Psig_value
                (Val.mk (mkrhs $startpos(v2) $endpos(v2) v2) v4 ~prim:v6 ~attrs:v7
                   ~loc:(rloc $startpos $endpos))) }
| TYPE @{`Item "type"}
  v2 = type_declarations
    { mksig $startpos $endpos (Psig_type (List.rev v2)) }
| TYPE @{`Item "type"}
  v2 = sig_type_extension
    { mksig $startpos $endpos (Psig_typext v2) }
| EXCEPTION @{`Item "exception"}
  v2 = sig_exception_declaration
    { mksig $startpos $endpos (Psig_exception v2) }
| MODULE @{`Item "module"}
  v2 = UIDENT v3 = module_declaration v4 = post_item_attributes
    { mksig $startpos $endpos (Psig_module (Md.mk (mkrhs $startpos(v2) $endpos(v2) v2)
                             v3 ~attrs:v4 ~loc:(rloc $startpos $endpos))) }
| MODULE @{`Item "module"}
  v2 = UIDENT EQUAL v4 = mod_longident v5 = post_item_attributes
    { mksig $startpos $endpos (Psig_module (Md.mk (mkrhs $startpos(v2) $endpos(v2) v2)
                             (Mty.alias ~loc:(rloc $startpos(v4) $endpos(v4)) (mkrhs $startpos(v4) $endpos(v4) v4))
                             ~attrs:v5
                             ~loc:(rloc $startpos $endpos)
                          )) }
| MODULE REC @{`Item "recursive module"}
  v3 = module_rec_declarations
    { mksig $startpos $endpos (Psig_recmodule (List.rev v3)) }
| MODULE TYPE @{`Item "module type"}
  v3 = ident v4 = post_item_attributes
    { mksig $startpos $endpos (Psig_modtype (Mtd.mk (mkrhs $startpos(v3) $endpos(v3) v3)
                              ~attrs:v4 ~loc:(rloc $startpos $endpos))) }
| MODULE TYPE @{`Item "module type"}
  v3 = ident EQUAL v5 = module_type v6 = post_item_attributes
    { mksig $startpos $endpos (Psig_modtype (Mtd.mk (mkrhs $startpos(v3) $endpos(v3) v3) ~typ:v5
                              ~loc:(rloc $startpos $endpos)
                              ~attrs:v6)) }
| v1 = open_statement
    { mksig $startpos $endpos (Psig_open v1) }
| INCLUDE @{`Item "include"}
  v2 = module_type v3 = post_item_attributes %prec below_WITH
    { mksig $startpos $endpos (Psig_include (Incl.mk v2 ~attrs:v3
                                             ~loc:(rloc $startpos $endpos))) }
| CLASS @{`Item "class"}
  v2 = class_descriptions
    { mksig $startpos $endpos (Psig_class (List.rev v2)) }
| CLASS TYPE @{`Item "class type"}
  v3 = class_type_declarations
    { mksig $startpos $endpos (Psig_class_type (List.rev v3)) }
| v1 = item_extension v2 = post_item_attributes
    { mksig $startpos $endpos (Psig_extension (v1, v2)) }
| v1 = floating_attribute
    { mksig $startpos $endpos (Psig_attribute v1) }

open_statement:
| OPEN @{`Item "open"}
  v2 = override_flag v3 = mod_longident v4 = post_item_attributes
    { Opn.mk (mkrhs $startpos(v3) $endpos(v3) v3) ~override:v2 ~attrs:v4
        ~loc:(rloc $startpos $endpos) }

module_declaration:
| COLON v2 = module_type
    { v2 }
| LPAREN v2 = UIDENT COLON v4 = module_type RPAREN v6 = module_declaration
    { mkmty $startpos $endpos (Pmty_functor(mkrhs $startpos(v2) $endpos(v2) v2, Some v4, v6)) }
| _1 = LPAREN RPAREN v3 = module_declaration
    { mkmty $startpos $endpos (Pmty_functor(mkrhs $startpos(_1) $endpos(_1) "()", None, v3)) }

module_rec_declarations:
| v1 = module_rec_declaration
    { [v1] }
| v1 = module_rec_declarations AND v3 = module_rec_declaration
    { v3 :: v1 }

module_rec_declaration:
| v1 = UIDENT COLON v3 = module_type v4 = post_item_attributes
    { Md.mk (mkrhs $startpos(v1) $endpos(v1) v1) v3 ~attrs:v4 ~loc:(rloc $startpos $endpos) }

(* Class expressions *)

class_declarations:
| v1 = class_declarations AND v3 = class_declaration
    { v3 @ v1 }
| v1 = class_declaration
    { v1 }

class_declaration:
| v1 = virtual_flag v2 = class_type_parameters v3 = LIDENT v4 = class_fun_binding
  v5 = post_item_attributes
    {
      [Ci.mk (mkrhs $startpos(v3) $endpos(v3) v3) v4
         ~virt:v1 ~params:v2
         ~attrs:v5 ~loc:(rloc $startpos $endpos)]
    }

class_fun_binding:
| EQUAL v2 = class_expr
    { v2 }
| COLON v2 = class_type EQUAL v4 = class_expr
    { mkclass $startpos $endpos (Pcl_constraint(v4, v2)) }
| v1 = labeled_simple_pattern v2 = class_fun_binding
    { let (l,o,p) = v1 in mkclass $startpos $endpos (Pcl_fun(l, o, p, v2)) }

class_type_parameters:
| (* empty *)
    { [] }
| LBRACKET v2 = type_parameter_list RBRACKET
    { List.rev v2 }

class_fun_def:
| v1 = labeled_simple_pattern MINUSGREATER v3 = class_expr
    { let (l,o,p) = v1 in mkclass $startpos $endpos (Pcl_fun(l, o, p, v3)) }
| v1 = labeled_simple_pattern v2 = class_fun_def
    { let (l,o,p) = v1 in mkclass $startpos $endpos (Pcl_fun(l, o, p, v2)) }

class_expr:
| v1 = class_simple_expr
    { v1 }
| FUN v2 = class_fun_def
    { v2 }
| v1 = class_simple_expr v2 = simple_labeled_expr_list
    { mkclass $startpos $endpos (Pcl_apply(v1, List.rev v2)) }
| LET v2 = rec_flag v3 = let_bindings_no_attrs IN @{`Shift 2} v5 = class_expr
    { mkclass $startpos $endpos (Pcl_let (v2, List.rev v3, v5)) }
| v1 = class_expr v2 = attribute
    { Cl.attr v1 v2 }
| v1 = extension
    { mkclass $startpos $endpos (Pcl_extension v1) }

class_simple_expr:
| LBRACKET v2 = core_type_comma_list RBRACKET v4 = class_longident
    { mkclass $startpos $endpos (Pcl_constr(mkloc v4 (rloc $startpos(v4) $endpos(v4)), List.rev v2)) }
| v1 = class_longident
    { mkclass $startpos $endpos (Pcl_constr(mkrhs $startpos(v1) $endpos(v1) v1, [])) }
| OBJECT @{`Unclosed "object"} @{`Item "object"} v2 = class_structure END
    { mkclass $startpos $endpos (Pcl_structure(v2)) }
| LPAREN @{`Unclosed "("} v2 = class_expr COLON v4 = class_type RPAREN
    { mkclass $startpos $endpos (Pcl_constraint(v2, v4)) }
| LPAREN @{`Unclosed "("} v2 = class_expr RPAREN
    { v2 }

class_structure:
| v1 = class_self_pattern v2 = class_fields
    { Cstr.mk v1 (List.rev v2) }

class_self_pattern:
| LPAREN v2 = pattern RPAREN
    { reloc_pat $startpos $endpos v2 }
| LPAREN v2 = pattern COLON v4 = core_type RPAREN
    { mkpat $startpos $endpos (Ppat_constraint(v2, v4)) }
| (* empty *)
    { ghpat $startpos $endpos (Ppat_any) }

class_fields:
| (* empty *)
    { [] }
| v1 = class_fields v2 = class_field
    { v2 @ v1 }

class_field:
| INHERIT v2 = override_flag v3 = class_expr v4 = parent_binder attrs = post_item_attributes
    { mkcf $startpos $endpos (Pcf_inherit (v2, v3, v4)) ~attrs }
| VAL v2 = value attrs = post_item_attributes
    { mkcf $startpos $endpos (Pcf_val v2) ~attrs }
| METHOD v2 = method_ attrs = post_item_attributes
    { mkcf $startpos $endpos (Pcf_method v2) ~attrs }
| CONSTRAINT v2 = constrain_field attrs = post_item_attributes
    { mkcf $startpos $endpos (Pcf_constraint v2) ~attrs }
| INITIALIZER v2 = seq_expr attrs = post_item_attributes
    { mkcf $startpos $endpos (Pcf_initializer v2) ~attrs }
| v1 = item_extension
    { mkcf $startpos $endpos (Pcf_extension v1) }
| v1 = floating_attribute
    { mkcf $startpos $endpos (Pcf_attribute v1) }

parent_binder:
| AS v2 = LIDENT
    { Some v2 }
| (* empty *)
    { None }

value:
(* TODO: factorize these rules (also with method): *)
| v1 = override_flag MUTABLE VIRTUAL v4 = label COLON v6 = core_type
    { if v1 = Override then syntax_error $startpos $endpos;
        mkloc v4 (rloc $startpos(v4) $endpos(v4)), Mutable, Cfk_virtual v6 }
| VIRTUAL v2 = mutable_flag v3 = label COLON v5 = core_type
    { mkrhs $startpos(v3) $endpos(v3) v3, v2, Cfk_virtual v5 }
| v1 = override_flag v2 = mutable_flag v3 = label EQUAL v5 = seq_expr
    { mkrhs $startpos(v3) $endpos(v3) v3, v2, Cfk_concrete (v1, v5) }
| v1 = override_flag v2 = mutable_flag v3 = label v4 = type_constraint EQUAL v6 = seq_expr
    {
       let e = mkexp_constraint $startpos $endpos v6 v4 in
       mkrhs $startpos(v3) $endpos(v3) v3, v2, Cfk_concrete (v1, e)
      }

method_:
(* TODO: factorize those rules... *)
| v1 = override_flag PRIVATE VIRTUAL v4 = label COLON v6 = poly_type
    { if v1 = Override then syntax_error $startpos $endpos;
        mkloc v4 (rloc $startpos(v4) $endpos(v4)), Private, Cfk_virtual v6 }
| v1 = override_flag VIRTUAL v3 = private_flag v4 = label COLON v6 = poly_type
    { if v1 = Override then syntax_error $startpos $endpos;
        mkloc v4 (rloc $startpos(v4) $endpos(v4)), v3, Cfk_virtual v6 }
| v1 = override_flag v2 = private_flag v3 = label v4 = strict_binding
    { mkloc v3 (rloc $startpos(v3) $endpos(v3)), v2, Cfk_concrete (v1, ghexp $startpos $endpos (Pexp_poly (v4, None))) }
| v1 = override_flag v2 = private_flag v3 = label COLON v5 = poly_type EQUAL v7 = seq_expr
    { mkloc v3 (rloc $startpos(v3) $endpos(v3)), v2, Cfk_concrete (v1, ghexp $startpos $endpos (Pexp_poly(v7, Some v5))) }
| v1 = override_flag v2 = private_flag v3 = label COLON TYPE v6 = lident_list DOT v8 = core_type EQUAL v10 = seq_expr
    { let exp, poly = wrap_type_annotation $startpos $endpos v6 v8 v10 in
        mkloc v3 (rloc $startpos(v3) $endpos(v3)), v2, Cfk_concrete (v1, ghexp $startpos $endpos (Pexp_poly(exp, Some poly))) }

(* Class types *)
class_type:
| v1 = class_signature
    { v1 }
| QUESTION v2 = LIDENT COLON v4 = simple_core_type_or_tuple_no_attr MINUSGREATER v6 = class_type
    { mkcty $startpos $endpos (Pcty_arrow("?" ^ v2 , mkoption v4, v6)) }
| v1 = OPTLABEL v2 = simple_core_type_or_tuple_no_attr MINUSGREATER v4 = class_type
    { mkcty $startpos $endpos (Pcty_arrow("?" ^ v1, mkoption v2, v4)) }
| v1 = LIDENT COLON v3 = simple_core_type_or_tuple_no_attr MINUSGREATER v5 = class_type
    { mkcty $startpos $endpos (Pcty_arrow(v1, v3, v5)) }
| v1 = simple_core_type_or_tuple_no_attr MINUSGREATER v3 = class_type
    { mkcty $startpos $endpos (Pcty_arrow("", v1, v3)) }
| v1 = class_type v2 = attribute
    { Cty.attr v1 v2 }
| v1 = extension
    { mkcty $startpos $endpos (Pcty_extension v1) }

class_signature:
| LBRACKET v2 = core_type_comma_list RBRACKET v4 = clty_longident
    { mkcty $startpos $endpos (Pcty_constr (mkloc v4 (rloc $startpos(v4) $endpos(v4)), List.rev v2)) }
| v1 = clty_longident
    { mkcty $startpos $endpos (Pcty_constr (mkrhs $startpos(v1) $endpos(v1) v1, [])) }
| OBJECT @{`Unclosed "object"} @{`Item "object"} v2 = class_sig_body END
    { mkcty $startpos $endpos (Pcty_signature v2) }

class_sig_body:
| v1 = class_self_type v2 = class_sig_fields
    { Csig.mk v1 (List.rev v2) }

class_self_type:
| LPAREN v2 = core_type RPAREN
    { v2 }
| (* empty *)
    { mktyp $startpos $endpos (Ptyp_any) }

class_sig_fields:
| (* empty *)
    { [] }
| v1 = class_sig_fields v2 = class_sig_field
    { v2 :: v1 }

class_sig_field:
| INHERIT v2 = class_signature attrs = post_item_attributes
    { mkctf $startpos $endpos  (Pctf_inherit v2) ~attrs }
| VAL v2 = value_type attrs = post_item_attributes
    { mkctf $startpos $endpos  (Pctf_val v2) ~attrs }
| METHOD v2 = private_virtual_flags v3 = label COLON v5 = poly_type attrs = post_item_attributes
    {
      let (p, v) = v2 in
      mkctf $startpos $endpos  (Pctf_method (v3, p, v, v5)) ~attrs
    }
| CONSTRAINT v2 = constrain_field attrs = post_item_attributes
    { mkctf $startpos $endpos  (Pctf_constraint v2) ~attrs }
| v1 = item_extension attrs = post_item_attributes
    { mkctf $startpos $endpos (Pctf_extension v1) ~attrs }
| v1 = floating_attribute
    { mkctf $startpos $endpos (Pctf_attribute v1) }

value_type:
| VIRTUAL v2 = mutable_flag v3 = label COLON v5 = core_type
    { v3, v2, Virtual, v5 }
| MUTABLE v2 = virtual_flag v3 = label COLON v5 = core_type
    { v3, Mutable, v2, v5 }
| v1 = label COLON v3 = core_type
    { v1, Immutable, Concrete, v3 }

constrain:
| v1 = core_type EQUAL v3 = core_type
    { v1, v3, (rloc $startpos $endpos) }

constrain_field:
| v1 = core_type EQUAL v3 = core_type
    { v1, v3 }

class_descriptions:
| v1 = class_descriptions AND v3 = class_description
    { v3 @ v1 }
| v1 = class_description
    { v1 }

class_description:
| v1 = virtual_flag v2 = class_type_parameters v3 = LIDENT COLON v5 = class_type v6 = post_item_attributes
    {
      [Ci.mk (mkrhs $startpos(v3) $endpos(v3) v3) v5
         ~virt:v1 ~params:v2
         ~attrs:v6 ~loc:(rloc $startpos $endpos)]
    }

class_type_declarations:
| v1 = class_type_declarations AND v3 = class_type_declaration
    { v3 @ v1 }
| v1 = class_type_declaration
    { v1 }

class_type_declaration:
| v1 = virtual_flag v2 = class_type_parameters v3 = LIDENT EQUAL v5 = class_signature v6 = post_item_attributes
    {
      [Ci.mk (mkrhs $startpos(v3) $endpos(v3) v3) v5
         ~virt:v1 ~params:v2
         ~attrs:v6 ~loc:(rloc $startpos $endpos)]
    }

(* Core expressions *)

seq_expr:
| v1 = expr %prec below_SEMI
    { v1 }
| v1 = expr SEMI
    { reloc_exp $startpos $endpos v1 }
| v1 = expr SEMI @{`Shift 1} v3 = seq_expr
    { mkexp $startpos $endpos (Pexp_sequence(v1, v3)) }

labeled_simple_pattern:
| QUESTION LPAREN v3 = label_let_pattern v4 = opt_default RPAREN
    { ("?" ^ fst v3, v4, snd v3) }
| QUESTION v2 = label_var
    { ("?" ^ fst v2, None, snd v2) }
| v1 = OPTLABEL LPAREN v3 = let_pattern v4 = opt_default RPAREN
    { ("?" ^ v1, v4, v3) }
| v1 = OPTLABEL v2 = pattern_var
    { ("?" ^ v1, None, v2) }
| TILDE LPAREN v3 = label_let_pattern RPAREN
    { (fst v3, None, snd v3) }
| TILDE v2 = label_var
    { (fst v2, None, snd v2) }
| v1 = LABEL v2 = simple_pattern
    { (v1, None, v2) }
| v1 = simple_pattern
    { ("", None, v1) }

pattern_var:
| v1 = LIDENT
    { mkpat $startpos $endpos (Ppat_var (mkrhs $startpos(v1) $endpos(v1) v1)) }
| UNDERSCORE
    { mkpat $startpos $endpos  Ppat_any }

opt_default:
| (* empty *)
    { None }
| EQUAL v2 = seq_expr
    { Some v2 }

label_let_pattern:
| v1 = label_var
    { v1 }
| v1 = label_var COLON v3 = core_type
    { let (lab, pat) = v1 in (lab, mkpat $startpos $endpos (Ppat_constraint(pat, v3))) }

label_var:
| v1 = LIDENT
    { (v1, mkpat $startpos $endpos (Ppat_var (mkrhs $startpos(v1) $endpos(v1) v1))) }

let_pattern:
| v1 = pattern
    { v1 }
| v1 = pattern COLON v3 = core_type
    { mkpat $startpos $endpos (Ppat_constraint(v1, v3)) }

expr:
| v1 = simple_expr %prec below_SHARP
    { v1 }
| v1 = simple_expr v2 = simple_labeled_expr_list
    { mkexp $startpos $endpos (Pexp_apply(v1, List.rev v2)) }
| LET @{`Item "let"} v2 = ext_attributes v3 = rec_flag v4 = let_bindings_no_attrs _in = IN @{`Shift 2} expr = seq_expr
    { let expr = reloc_exp_fake $endpos(_in) $endpos expr in
      mkexp_attrs $startpos $endpos (Pexp_let(v3, List.rev v4, expr)) v2 }
| LET MODULE @{`Item "let module"}
  v3 = ext_attributes v4 = UIDENT v5 = module_binding_body _in = IN @{`Shift 2} expr = seq_expr
    { let expr = reloc_exp_fake $endpos(_in) $endpos expr in
      mkexp_attrs $startpos $endpos (Pexp_letmodule(mkrhs $startpos(v4) $endpos(v4) v4, v5, expr)) v3 }
| LET OPEN @{`Item "let open"}
  v3 = expr_open _in = IN @{`Shift 2} expr = seq_expr
    { let expr = reloc_exp_fake $endpos(_in) $endpos expr in
      let (flag,id,ext) = v3 in
      mkexp_attrs $startpos $endpos (Pexp_open(flag, id, expr)) ext }
| FUNCTION @{`Item "function"}
  v2 = ext_attributes opt_bar v4 = match_cases
    { mkexp_attrs $startpos $endpos (Pexp_function(List.rev v4)) v2 }
| FUN @{`Item "fun"}
  v2 = ext_attributes v3 = labeled_simple_pattern v4 = fun_def
    { let (l,o,p) = v3 in
        mkexp_attrs $startpos $endpos (Pexp_fun(l, o, p, v4)) v2 }
| FUN @{`Item "fun"}
  v2 = ext_attributes v3 = newtype v4 = fun_def
    { mkexp_attrs $startpos $endpos (Pexp_newtype(v3, v4)) v2 }
| MATCH @{`Item "match"}
  v2 = ext_attributes v3 = seq_expr WITH opt_bar v6 = match_cases
    { mkexp_attrs $startpos $endpos (Pexp_match(v3, List.rev v6)) v2 }
| TRY @{`Item "try"}
  v2 = ext_attributes v3 = seq_expr WITH opt_bar v6 = match_cases
    { mkexp_attrs $startpos $endpos (Pexp_try(v3, List.rev v6)) v2 }
| v1 = expr_comma_list %prec below_COMMA
    { mkexp $startpos $endpos (Pexp_tuple(List.rev v1)) }
| v1 = constr_longident v2 = simple_expr %prec below_SHARP
    { mkexp $startpos $endpos (Pexp_construct(mkrhs $startpos(v1) $endpos(v1) v1, Some v2)) }
| v1 = name_tag v2 = simple_expr %prec below_SHARP
    { mkexp $startpos $endpos (Pexp_variant(v1, Some v2)) }
| IF @{`Item "if"}
  v2 = ext_attributes v3 = seq_expr
  THEN @{`Item "then clause"} v5 = expr
  ELSE @{`Item "else clause"} v7 = expr
    { mkexp_attrs $startpos $endpos (Pexp_ifthenelse(v3, v5, Some v7)) v2 }
| IF @{`Item "if"}
  v2 = ext_attributes v3 = seq_expr
  THEN @{`Item "then clause"} v5 = expr
    { mkexp_attrs $startpos $endpos (Pexp_ifthenelse(v3, v5, None)) v2 }
| WHILE @{`Item "while"}
  v2 = ext_attributes v3 = seq_expr DO @{`Item "while body"} v5 = seq_expr DONE
    { mkexp_attrs $startpos $endpos (Pexp_while(v3, v5)) v2 }
| FOR @{`Item "for"}
  v2 = ext_attributes v3 = pattern EQUAL v5 = seq_expr v6 = direction_flag v7 = seq_expr
  DO @{`Item "for body"} v9 = seq_expr DONE
    { mkexp_attrs $startpos $endpos (Pexp_for(v3, v5, v7, v6, v9)) v2 }
| v1 = expr _2 = COLONCOLON v3 = expr
    { mkexp_cons (rloc $startpos(_2) $endpos(_2)) (ghexp $startpos $endpos (Pexp_tuple[v1;v3])) (rloc $startpos $endpos) }
| LPAREN _2 = COLONCOLON RPAREN LPAREN v5 = expr COMMA v7 = expr RPAREN
    { mkexp_cons (rloc $startpos(_2) $endpos(_2)) (ghexp $startpos $endpos (Pexp_tuple[v5;v7])) (rloc $startpos $endpos) }
| v1 = expr v2 = INFIXOP0 v3 = expr
    { mkinfix $startpos $endpos v1 $startpos(v2) $endpos(v2) v2 v3 }
| v1 = expr v2 = INFIXOP1 v3 = expr
    { mkinfix $startpos $endpos v1 $startpos(v2) $endpos(v2) v2 v3 }
| v1 = expr v2 = INFIXOP2 v3 = expr
    { mkinfix $startpos $endpos v1 $startpos(v2) $endpos(v2) v2 v3 }
| v1 = expr v2 = INFIXOP3 v3 = expr
    { mkinfix $startpos $endpos v1 $startpos(v2) $endpos(v2) v2 v3 }
| v1 = expr v2 = INFIXOP4 v3 = expr
    { mkinfix $startpos $endpos v1 $startpos(v2) $endpos(v2) v2 v3 }
| v1 = expr _2 = PLUS v3 = expr
    { mkinfix $startpos $endpos v1 $startpos(_2) $endpos(_2) "+" v3 }
| v1 = expr _2 = PLUSDOT v3 = expr
    { mkinfix $startpos $endpos v1 $startpos(_2) $endpos(_2) "+." v3 }
| v1 = expr _2 = PLUSEQ v3 = expr
    { mkinfix $startpos $endpos v1 $startpos(_2) $endpos(_2) "+=" v3 }
| v1 = expr _2 = MINUS v3 = expr
    { mkinfix $startpos $endpos v1 $startpos(_2) $endpos(_2) "-" v3 }
| v1 = expr _2 = MINUSDOT v3 = expr
    { mkinfix $startpos $endpos v1 $startpos(_2) $endpos(_2) "-." v3 }
| v1 = expr _2 = STAR v3 = expr
    { mkinfix $startpos $endpos v1 $startpos(_2) $endpos(_2) "*" v3 }
| v1 = expr _2 = PERCENT v3 = expr
    { mkinfix $startpos $endpos v1 $startpos(_2) $endpos(_2) "%" v3 }
| v1 = expr _2 = EQUAL v3 = expr
    { mkinfix $startpos $endpos v1 $startpos(_2) $endpos(_2) "=" v3 }
| v1 = expr _2 = LESS v3 = expr
    { mkinfix $startpos $endpos v1 $startpos(_2) $endpos(_2) "<" v3 }
| v1 = expr _2 = GREATER v3 = expr
    { mkinfix $startpos $endpos v1 $startpos(_2) $endpos(_2) ">" v3 }
| v1 = expr _2 = OR v3 = expr
    { mkinfix $startpos $endpos v1 $startpos(_2) $endpos(_2) "or" v3 }
| v1 = expr _2 = BARBAR v3 = expr
    { mkinfix $startpos $endpos v1 $startpos(_2) $endpos(_2) "||" v3 }
| v1 = expr _2 = AMPERSAND v3 = expr
    { mkinfix $startpos $endpos v1 $startpos(_2) $endpos(_2) "&" v3 }
| v1 = expr _2 = AMPERAMPER v3 = expr
    { mkinfix $startpos $endpos v1 $startpos(_2) $endpos(_2) "&&" v3 }
| v1 = expr _2 = COLONEQUAL v3 = expr
    { mkinfix $startpos $endpos v1 $startpos(_2) $endpos(_2) ":=" v3 }
| v1 = subtractive v2 = expr %prec prec_unary_minus
    { mkuminus $startpos $endpos v1 v2 }
| v1 = additive v2 = expr %prec prec_unary_plus
    { mkuplus $startpos $endpos v1 v2 }
| v1 = simple_expr DOT v3 = label_longident LESSMINUS v5 = expr
    { mkexp $startpos $endpos (Pexp_setfield(v1, mkrhs $startpos(v3) $endpos(v3) v3, v5)) }
| v1 = simple_expr _ops = DOT _ope = LPAREN v4 = seq_expr RPAREN LESSMINUS v7 = expr
    { mkexp $startpos $endpos
          (Pexp_apply(ghexp $startpos(_ops) $endpos(_ope)
                 (Pexp_ident(array_function $startpos(_ops) $endpos(_ope) "Array" "set")),
                         ["",v1; "",v4; "",v7])) }
| v1 = simple_expr _ops = DOT _ope = LBRACKET v4 = seq_expr RBRACKET LESSMINUS v7 = expr
    { mkexp $startpos $endpos
          (Pexp_apply(ghexp $startpos(_ops) $endpos(_ope)
                 (Pexp_ident(array_function $startpos(_ops) $endpos(_ope) "String" "set")),
                         ["",v1; "",v4; "",v7])) }
| v1 = simple_expr _ops = DOT _ope = LBRACE v4 = expr RBRACE LESSMINUS v7 = expr
    { bigarray_set ($startpos,$endpos) ($startpos(_ops),$endpos(_ope)) v1 v4 v7 }
| v1 = label LESSMINUS v3 = expr
    { mkexp $startpos $endpos (Pexp_setinstvar(mkrhs $startpos(v1) $endpos(v1) v1, v3)) }
| ASSERT v2 = ext_attributes v3 = simple_expr %prec below_SHARP
    { mkexp_attrs $startpos $endpos (Pexp_assert v3) v2 }
| LAZY v2 = ext_attributes v3 = simple_expr %prec below_SHARP
    { mkexp_attrs $startpos $endpos (Pexp_lazy v3) v2 }
| OBJECT @{`Unclosed "object"} @{`Item "object"} v2 = ext_attributes v3 = class_structure END
    { mkexp_attrs $startpos $endpos (Pexp_object v3) v2 }
| v1 = expr v2 = attribute
    { Exp.attr v1 v2 }

simple_expr:
| v1 = val_longident
    { mkexp $startpos $endpos (Pexp_ident (mkrhs $startpos(v1) $endpos(v1) v1)) }
| v1 = constant
    { mkexp $startpos $endpos (Pexp_constant v1) }
| v1 = constr_longident %prec prec_constant_constructor
    { mkexp $startpos $endpos (Pexp_construct(mkrhs $startpos(v1) $endpos(v1) v1, None)) }
| v1 = name_tag %prec prec_constant_constructor
    { mkexp $startpos $endpos (Pexp_variant(v1, None)) }
| LPAREN @{`Unclosed "("} v2 = seq_expr RPAREN
    { reloc_exp $startpos $endpos v2 }
| BEGIN v2 = ext_attributes v3 = seq_expr END
    { wrap_exp_attrs $startpos $endpos (reloc_exp $startpos $endpos v3) v2 (* check location *) }
| BEGIN @{`Unclosed "begin"} v2 = ext_attributes END
    { mkexp_attrs $startpos $endpos (Pexp_construct (mkloc (Lident "()") (rloc $startpos $endpos),
                               None)) v2 }
| LPAREN v2 = seq_expr v3 = type_constraint RPAREN
    { mkexp_constraint $startpos $endpos v2 v3 }
| v1 = simple_expr DOT v3 = label_longident
    { mkexp $startpos $endpos (Pexp_field(v1, mkrhs $startpos(v3) $endpos(v3) v3)) }
| v1 = mod_longident DOT LPAREN @{`Unclosed "("} v4 = seq_expr RPAREN
    { mkexp $startpos $endpos (Pexp_open(Fresh, mkrhs $startpos(v1) $endpos(v1) v1, v4)) }
| v1 = simple_expr _ops = DOT _ope = LPAREN @{`Unclosed "("} v4 = seq_expr RPAREN
    { mkexp $startpos $endpos
          (Pexp_apply(ghexp $startpos(_ops) $endpos(_ope)
                 (Pexp_ident(array_function $startpos(_ops) $endpos(_ope) "Array" "get")),
                         ["",v1; "",v4])) }
| v1 = simple_expr _ops = DOT _ope = LBRACKET @{`Unclosed "["} v4 = seq_expr RBRACKET
    { mkexp $startpos $endpos
          (Pexp_apply(ghexp $startpos(_ops) $endpos(_ope)
                 (Pexp_ident(array_function $startpos(_ops) $endpos(_ope) "String" "get")),
                         ["",v1; "",v4])) }
| v1 = simple_expr _ops = DOT _ope = LBRACE @{`Unclosed "{"} v4 = expr RBRACE
    { bigarray_get ($startpos,$endpos) ($startpos(_ops),$endpos(_ope)) v1 v4 }
| LBRACE @{`Unclosed "{"} v2 = record_expr RBRACE
    { let (exten, fields) = v2 in mkexp $startpos $endpos (Pexp_record(fields, exten)) }
| v1 = mod_longident DOT LBRACE @{`Unclosed "{"}  v4 = record_expr RBRACE
    { let (exten, fields) = v4 in
        let rec_exp = mkexp $startpos $endpos (Pexp_record(fields, exten)) in
        mkexp $startpos $endpos (Pexp_open(Fresh, mkrhs $startpos(v1) $endpos(v1) v1, rec_exp)) }
| LBRACKETBAR @{`Unclosed "[|"} v2 = expr_semi_list opt_semi BARRBRACKET
    { mkexp $startpos $endpos  (Pexp_array(List.rev v2)) }
| LBRACKETBAR BARRBRACKET
    { mkexp $startpos $endpos  (Pexp_array []) }
| v1 = mod_longident DOT LBRACKETBAR @{`Unclosed "[|"} v4 = expr_semi_list opt_semi BARRBRACKET
    { mkexp $startpos $endpos (Pexp_open(Fresh, mkrhs $startpos(v1) $endpos(v1) v1, mkexp $startpos(v4) $endpos(v4) (Pexp_array(List.rev v4)))) }
| LBRACKET @{`Unclosed "["} v2 = expr_semi_list opt_semi _4 = RBRACKET
    { reloc_exp $startpos $endpos (mktailexp $startpos(_4) $endpos(_4) (List.rev v2)) }
| v1 = mod_longident DOT LBRACKET @{`Unclosed "["} v4 = expr_semi_list opt_semi _6 = RBRACKET
    { let list_exp = reloc_exp $startpos $endpos (mktailexp $startpos(_6) $endpos(_6) (List.rev v4)) in
        mkexp $startpos $endpos (Pexp_open(Fresh, mkrhs $startpos(v1) $endpos(v1) v1, list_exp)) }
| v1 = PREFIXOP v2 = simple_expr
    { mkexp $startpos $endpos (Pexp_apply(mkoperator $startpos(v1) $endpos(v1) v1, ["",v2])) }
| _1 = BANG v2 = simple_expr
    { mkexp $startpos $endpos (Pexp_apply(mkoperator $startpos(_1) $endpos(_1) "!", ["",v2])) }
| NEW v2 = ext_attributes v3 = class_longident
    { mkexp_attrs $startpos $endpos (Pexp_new(mkrhs $startpos(v3) $endpos(v3) v3)) v2 }
| LBRACELESS @{`Unclosed "{<"} v2 = field_expr_list opt_semi GREATERRBRACE
    { mkexp $startpos $endpos  (Pexp_override(List.rev v2)) }
| LBRACELESS GREATERRBRACE
    { mkexp $startpos $endpos  (Pexp_override [])}
| v1 = mod_longident DOT LBRACELESS @{`Unclosed "{<"} v4 = field_expr_list opt_semi GREATERRBRACE
    { mkexp $startpos $endpos (Pexp_open(Fresh, mkrhs $startpos(v1) $endpos(v1) v1, mkexp $startpos(v4) $endpos(v4) (Pexp_override(List.rev v4)))) }
| v1 = simple_expr SHARP @{`Shift_token (1,LIDENT "")} v3 = label
    { mkexp $startpos $endpos (Pexp_send(v1, v3)) }
| LPAREN @{`Unclosed "("} MODULE v3 = module_expr RPAREN
    { mkexp $startpos $endpos  (Pexp_pack v3) }
| LPAREN @{`Unclosed "("} MODULE v3 = module_expr COLON v5 = package_type RPAREN
    { mkexp $startpos $endpos  (Pexp_constraint (ghexp $startpos $endpos (Pexp_pack v3),
                                ghtyp $startpos $endpos (Ptyp_package v5))) }
| v1 = mod_longident DOT LPAREN @{`Unclosed "("} MODULE v5 = module_expr COLON v7 = package_type RPAREN
    { mkexp $startpos $endpos (Pexp_open(Fresh, mkrhs $startpos(v1) $endpos(v1) v1,
        mkexp $startpos $endpos (Pexp_constraint (ghexp $startpos $endpos (Pexp_pack v5),
                                ghtyp $startpos $endpos (Ptyp_package v7))))) }
| v1 = extension
    { mkexp $startpos $endpos  (Pexp_extension v1) }

simple_labeled_expr_list:
| v1 = labeled_simple_expr
    { [v1] }
| v1 = simple_labeled_expr_list v2 = labeled_simple_expr
    { v2 :: v1 }

labeled_simple_expr:
| v1 = simple_expr %prec below_SHARP
    { ("", v1) }
| v1 = label_expr
    { v1 }

label_expr:
| v1 = LABEL v2 = simple_expr %prec below_SHARP
    { (v1, v2) }
| TILDE v2 = label_ident
    { v2 }
| QUESTION v2 = label_ident
    { ("?" ^ fst v2, snd v2) }
| v1 = OPTLABEL v2 = simple_expr %prec below_SHARP
    { ("?" ^ v1, v2) }

label_ident:
| v1 = LIDENT
    { (v1, mkexp $startpos $endpos (Pexp_ident(mkrhs $startpos(v1) $endpos(v1) (Lident v1)))) }

let_bindings:
| v1 = let_binding
    { [v1] }
| v1 = let_bindings AND v3 = let_binding
    { v3 :: v1 }

let_bindings_no_attrs:
| l = let_bindings
    { List.iter (fun vb -> if vb.pvb_attributes <> [] then
        Parsing_aux.raise_warning
          (Syntaxerr.(Error(Not_expecting(vb.pvb_loc,"item attribute")))))
        l;
      l }

lident_list:
| v1 = LIDENT
    { [v1] }
| v1 = LIDENT v2 = lident_list
    { v1 :: v2 }

let_binding:
| v1 = let_binding_ v2 = post_item_attributes
    { let (p, e) = v1 in Vb.mk ~loc:(rloc $startpos $endpos) ~attrs:v2 p e }

let_binding_:
| v1 = val_ident v2 = fun_binding
    { (mkpatvar $startpos(v1) $endpos(v1) v1, v2) }
| v1 = val_ident COLON v3 = typevar_list DOT v5 = core_type EQUAL v7 = seq_expr
    { (ghpat $startpos $endpos (Ppat_constraint(mkpatvar $startpos(v1) $endpos(v1) v1,
                               ghtyp $startpos $endpos (Ptyp_poly(List.rev v3,v5)))),
         v7) }
| v1 = val_ident COLON TYPE v4 = lident_list DOT v6 = core_type EQUAL v8 = seq_expr
    { let exp, poly = wrap_type_annotation $startpos $endpos v4 v6 v8 in
        (ghpat $startpos $endpos (Ppat_constraint(mkpatvar $startpos(v1) $endpos(v1) v1, poly)), exp) }
| v1 = pattern EQUAL v3 = seq_expr
    { (v1, v3) }
| v1 = simple_pattern_not_ident COLON v3 = core_type EQUAL v5 = seq_expr
    { (ghpat $startpos $endpos (Ppat_constraint(v1, v3)), v5) }

fun_binding:
| v1 = strict_binding
    { v1 }
| v1 = type_constraint EQUAL v3 = seq_expr
    { mkexp_constraint $startpos $endpos v3 v1 }

strict_binding:
| EQUAL v2 = seq_expr
    { v2 }
| v1 = labeled_simple_pattern v2 = fun_binding
    { let (l, o, p) = v1 in ghexp $startpos $endpos (Pexp_fun(l, o, p, v2)) }
| LPAREN TYPE v3 = LIDENT RPAREN v5 = fun_binding
    { mkexp $startpos $endpos (Pexp_newtype(v3, v5)) }

match_cases:
| v1 = match_case
    { [v1] }
| v1 = match_cases @{`Indent (-2)} BAR v3 = match_case
    { v3 :: v1 }

match_case:
| @{`Item "pattern"} v1 = pattern
  _2 = MINUSGREATER @{`Item "match action"} expr = seq_expr
    { Exp.case v1 (reloc_exp_fake $endpos(_2) $endpos expr) }
| @{`Item "pattern"} v1 = pattern
  WHEN @{`Item "when guard"} v3 = seq_expr
  _4 = MINUSGREATER @{`Item "match action"} expr = seq_expr
    { Exp.case v1 ~guard:v3 (reloc_exp_fake $endpos(_4) $endpos expr) }

fun_def:
| MINUSGREATER v2 = seq_expr
(* Cf #5939: we used to accept (fun p when e0 -> e) *)
    { v2 }
| v1 = labeled_simple_pattern v2 = fun_def
    {
       let (l,o,p) = v1 in
       ghexp $startpos $endpos (Pexp_fun(l, o, p, v2))
      }
| LPAREN TYPE v3 = LIDENT RPAREN v5 = fun_def
    { mkexp $startpos $endpos (Pexp_newtype(v3, v5)) }

expr_comma_list:
| v1 = expr_comma_list COMMA v3 = expr
    { v3 :: v1 }
| v1 = expr COMMA v3 = expr
    { [v3; v1] }

record_expr:
| v1 = simple_expr WITH v3 = lbl_expr_list
    { (Some v1, v3) }
| v1 = lbl_expr_list
    { (None, v1) }

lbl_expr_list:
| v1 = lbl_expr
    { [v1] }
| v1 = lbl_expr SEMI v3 = lbl_expr_list
    { v1 :: v3 }
| v1 = lbl_expr SEMI
    { [v1] }

lbl_expr:
| v1 = label_longident EQUAL v3 = expr
    { (mkrhs $startpos(v1) $endpos(v1) v1,v3) }
| v1 = label_longident
    { (mkrhs $startpos(v1) $endpos(v1) v1, exp_of_label $startpos(v1) $endpos(v1) v1) }

field_expr_list:
| v1 = label EQUAL v3 = expr
    { [mkrhs $startpos(v1) $endpos(v1) v1,v3] }
| v1 = field_expr_list SEMI v3 = label EQUAL v5 = expr
    { (mkrhs $startpos(v3) $endpos(v3) v3, v5) :: v1 }

expr_semi_list:
| v1 = expr
    { [v1] }
| v1 = expr_semi_list SEMI v3 = expr
    { v3 :: v1 }

type_constraint:
| COLON @{`Item "type constraint"}
  v2 = core_type
    { (Some v2, None) }
| COLON @{`Item "type constraint"}
  v2 = core_type COLONGREATER v4 = core_type
    { (Some v2, Some v4) }
| COLONGREATER @{`Item "type constraint"}
  v2 = core_type
    { (None, Some v2) }

(* Patterns *)

pattern:
| v1 = simple_pattern
    { v1 }
| v1 = pattern AS v3 = val_ident
    { mkpat $startpos $endpos (Ppat_alias(v1, mkrhs $startpos(v3) $endpos(v3) v3)) }
(*| pattern AS v3 = error
    { expecting $startpos(v3) $endpos(v3) "identifier" }*)
| v1 = pattern_comma_list %prec below_COMMA
    { mkpat $startpos $endpos (Ppat_tuple(List.rev v1)) }
| v1 = constr_longident v2 = pattern %prec prec_constr_appl
    { mkpat $startpos $endpos (Ppat_construct(mkrhs $startpos(v1) $endpos(v1) v1, Some v2)) }
| v1 = name_tag v2 = pattern %prec prec_constr_appl
    { mkpat $startpos $endpos (Ppat_variant(v1, Some v2)) }
| v1 = pattern _2 = COLONCOLON v3 = pattern
    { mkpat_cons (rloc $startpos(_2) $endpos(_2)) (ghpat $startpos $endpos (Ppat_tuple[v1;v3])) (rloc $startpos $endpos) }
(*| pattern COLONCOLON v3 = error
    { expecting $startpos(v3) $endpos(v3) "pattern" }*)
| LPAREN _2 = COLONCOLON RPAREN LPAREN v5 = pattern COMMA v7 = pattern RPAREN
    { mkpat_cons (rloc $startpos(_2) $endpos(_2)) (ghpat $startpos $endpos (Ppat_tuple[v5;v7])) (rloc $startpos $endpos) }
| v1 = pattern BAR v3 = pattern
    { mkpat $startpos $endpos (Ppat_or(v1, v3)) }
(*| pattern BAR v3 = error
    { expecting $startpos(v3) $endpos(v3) "pattern" }*)
| LAZY v2 = simple_pattern
    { mkpat $startpos $endpos (Ppat_lazy v2) }
| EXCEPTION v2 = pattern %prec prec_constr_appl
    { mkpat $startpos $endpos (Ppat_exception v2) }
| v1 = pattern v2 = attribute
    { Pat.attr v1 v2 }

simple_pattern:
| v1 = val_ident %prec below_EQUAL
    { mkpat $startpos $endpos (Ppat_var (mkrhs $startpos(v1) $endpos(v1) v1)) }
| v1 = simple_pattern_not_ident
    { v1 }

simple_pattern_not_ident:
| UNDERSCORE
    { mkpat $startpos $endpos (Ppat_any) }
| v1 = signed_constant
    { mkpat $startpos $endpos (Ppat_constant v1) }
| v1 = signed_constant DOTDOT v3 = signed_constant
    { mkpat $startpos $endpos (Ppat_interval (v1, v3)) }
| v1 = constr_longident
    { mkpat $startpos $endpos (Ppat_construct(mkrhs $startpos(v1) $endpos(v1) v1, None)) }
| v1 = name_tag
    { mkpat $startpos $endpos (Ppat_variant(v1, None)) }
| SHARP v2 = type_longident
    { mkpat $startpos $endpos (Ppat_type (mkrhs $startpos(v2) $endpos(v2) v2)) }
| LBRACE @{`Unclosed "{"}
  v2 = lbl_pattern_list RBRACE
    { let (fields, closed) = v2 in mkpat $startpos $endpos (Ppat_record(fields, closed)) }
| LBRACKET @{`Unclosed "["}
  v2 = pattern_semi_list opt_semi _4 = RBRACKET
    { reloc_pat $startpos $endpos (mktailpat $startpos(_4) $endpos(_4) (List.rev v2)) }
| LBRACKETBAR @{`Unclosed "[|"}
  v2 = pattern_semi_list opt_semi BARRBRACKET
    { mkpat $startpos $endpos (Ppat_array(List.rev v2)) }
| LBRACKETBAR BARRBRACKET
    { mkpat $startpos $endpos (Ppat_array []) }
| LPAREN @{`Unclosed "("} v2 = pattern RPAREN
    { reloc_pat $startpos $endpos v2 }
| LPAREN @{`Unclosed "("} v2 = pattern COLON v4 = core_type RPAREN
    { mkpat $startpos $endpos (Ppat_constraint(v2, v4)) }
(*| LPAREN pattern COLON v4 = error
    { expecting $startpos(v4) $endpos(v4) "type" }*)
| LPAREN MODULE @{`Unclosed "("} v3 = UIDENT RPAREN
    { mkpat $startpos $endpos (Ppat_unpack (mkrhs $startpos(v3) $endpos(v3) v3)) }
| LPAREN MODULE @{`Unclosed "("} v3 = UIDENT COLON v5 = package_type RPAREN
    { mkpat $startpos $endpos (Ppat_constraint(mkpat $startpos $endpos(Ppat_unpack (mkrhs $startpos(v3) $endpos(v3) v3)),
                              ghtyp $startpos $endpos (Ptyp_package v5))) }
| v1 = extension
    { mkpat $startpos $endpos (Ppat_extension v1) }

pattern_comma_list:
| v1 = pattern_comma_list COMMA v3 = pattern
    { v3 :: v1 }
| v1 = pattern COMMA v3 = pattern
    { [v3; v1] }
(*| pattern COMMA v3 = error
    { expecting $startpos(v3) $endpos(v3) "pattern" }*)

pattern_semi_list:
| v1 = pattern
    { [v1] }
| v1 = pattern_semi_list SEMI v3 = pattern
    { v3 :: v1 }

lbl_pattern_list:
| v1 = lbl_pattern
    { [v1], Closed }
| v1 = lbl_pattern SEMI
    { [v1], Closed }
| v1 = lbl_pattern SEMI UNDERSCORE opt_semi
    { [v1], Open }
| v1 = lbl_pattern SEMI v3 = lbl_pattern_list
    { let (fields, closed) = v3 in v1 :: fields, closed }

lbl_pattern:
| v1 = label_longident EQUAL v3 = pattern
    { (mkrhs $startpos(v1) $endpos(v1) v1,v3) }
| v1 = label_longident
    { (mkrhs $startpos(v1) $endpos(v1) v1, pat_of_label $startpos(v1) $endpos(v1) v1) }

(* Primitive declarations *)

primitive_declaration:
| v1 = STRING
    { [fst v1] }
| v1 = STRING v2 = primitive_declaration
    { fst v1 :: v2 }

(* Type declarations *)

type_declarations:
| v1 = type_declaration
    { [v1] }
| v1 = type_declarations AND v3 = type_declaration
    { v3 :: v1 }

type_declaration:
| v1 = optional_type_parameters v2 = LIDENT v3 = type_kind v4 = constraints v5 = post_item_attributes
    { let (kind, priv, manifest) = v3 in
        Type.mk (mkrhs $startpos(v2) $endpos(v2) v2)
          ~params:v1 ~cstrs:(List.rev v4)
          ~kind ~priv ?manifest ~attrs:v5 ~loc:(rloc $startpos $endpos)
       }

constraints:
| v1 = constraints CONSTRAINT v3 = constrain
    { v3 :: v1 }
| (* empty *)
    { [] }

type_kind:
| (* empty *)
    { (Ptype_abstract, Public, None) }
| EQUAL v2 = core_type
    { (Ptype_abstract, Public, Some v2) }
| EQUAL PRIVATE v3 = core_type
    { (Ptype_abstract, Private, Some v3) }
| EQUAL v2 = constructor_declarations
    { (Ptype_variant(List.rev v2), Public, None) }
| EQUAL PRIVATE v3 = constructor_declarations
    { (Ptype_variant(List.rev v3), Private, None) }
| EQUAL v2 = private_flag BAR v4 = constructor_declarations
    { (Ptype_variant(List.rev v4), v2, None) }
| EQUAL v2 = private_flag LBRACE v4 = label_declarations opt_semi RBRACE
    { (Ptype_record(List.rev v4), v2, None) }
| EQUAL v2 = core_type EQUAL v4 = private_flag opt_bar v6 = constructor_declarations
    { (Ptype_variant(List.rev v6), v4, Some v2) }
| EQUAL v2 = core_type EQUAL v4 = private_flag LBRACE v6 = label_declarations opt_semi RBRACE
    { (Ptype_record(List.rev v6), v4, Some v2) }
| EQUAL DOTDOT
    { (Ptype_open, Public, None) }
| EQUAL v2 = core_type EQUAL DOTDOT
    { (Ptype_open, Public, Some v2) }

optional_type_parameters:
| (* empty *)
    { [] }
| v1 = optional_type_parameter
    { [v1] }
| LPAREN v2 = optional_type_parameter_list RPAREN
    { List.rev v2 }

optional_type_parameter:
| v1 = type_variance v2 = optional_type_variable
    { v2, v1 }

optional_type_parameter_list:
| v1 = optional_type_parameter
    { [v1] }
| v1 = optional_type_parameter_list COMMA v3 = optional_type_parameter
    { v3 :: v1 }

optional_type_variable:
| QUOTE v2 = ident
    { mktyp $startpos $endpos (Ptyp_var v2) }
| UNDERSCORE
    { mktyp $startpos $endpos (Ptyp_any) }

type_parameters:
| (* empty *)
    { [] }
| v1 = type_parameter
    { [v1] }
| LPAREN v2 = type_parameter_list RPAREN
    { List.rev v2 }

type_parameter:
| v1 = type_variance v2 = type_variable
    { v2, v1 }

type_variance:
| (* empty *)
    { Invariant }
| PLUS
    { Covariant }
| MINUS
    { Contravariant }

type_variable:
| QUOTE v2 = ident
    { mktyp $startpos $endpos (Ptyp_var v2) }

type_parameter_list:
| v1 = type_parameter
    { [v1] }
| v1 = type_parameter_list COMMA v3 = type_parameter
    { v3 :: v1 }

constructor_declarations:
| v1 = constructor_declaration
    { [v1] }
| v1 = constructor_declarations @{`Indent (-2)} BAR v3 = constructor_declaration
    { v3 :: v1 }

constructor_declaration:
| v1 = constr_ident v2 = attributes v3 = generalized_constructor_arguments
    {
      let args,res = v3 in
      Type.constructor (mkrhs $startpos(v1) $endpos(v1) v1) ~args ?res ~loc:(rloc $startpos $endpos) ~attrs:v2
    }

str_exception_declaration:
| v1 = extension_constructor_declaration v2 = post_item_attributes
    {
      let ext = v1 in
      {ext with pext_attributes = ext.pext_attributes @ v2}
    }
| v1 = extension_constructor_rebind v2 = post_item_attributes
    {
      let ext = v1 in
      {ext with pext_attributes = ext.pext_attributes @ v2}
    }

sig_exception_declaration:
| v1 = extension_constructor_declaration v2 = post_item_attributes
    {
      let ext = v1 in
      {ext with pext_attributes = ext.pext_attributes @ v2}
    }

generalized_constructor_arguments:
| (* empty *)
    { ([],None) }
| OF v2 = core_type_list
    { (List.rev v2,None) }
| COLON v2 = core_type_list MINUSGREATER v4 = simple_core_type
    { (List.rev v2,Some v4) }
| COLON v2 = simple_core_type
    { ([],Some v2) }

label_declarations:
| v1 = label_declaration
    { [v1] }
| v1 = label_declarations SEMI v3 = label_declaration
    { v3 :: v1 }

label_declaration:
| v1 = mutable_flag v2 = label v3 = attributes COLON v5 = poly_type
    {
       Type.field (mkrhs $startpos(v2) $endpos(v2) v2) v5 ~mut:v1 ~attrs:v3 ~loc:(rloc $startpos $endpos)
      }

(* Type extensions *)

str_type_extension:
| v1 = optional_type_parameters v2 = type_longident
  PLUSEQ v4 = private_flag opt_bar v6 = str_extension_constructors
  v7 = post_item_attributes
    { Te.mk (mkrhs $startpos(v2) $endpos(v2) v2) (List.rev v6)
        ~params:v1 ~priv:v4 ~attrs:v7 }

sig_type_extension:
| v1 = optional_type_parameters v2 = type_longident
  PLUSEQ v4 = private_flag opt_bar v6 = sig_extension_constructors
  v7 = post_item_attributes
    { Te.mk (mkrhs $startpos(v2) $endpos(v2) v2) (List.rev v6)
        ~params:v1 ~priv:v4 ~attrs:v7 }

str_extension_constructors:
| v1 = extension_constructor_declaration
    { [v1] }
| v1 = extension_constructor_rebind
    { [v1] }
| v1 = str_extension_constructors BAR v3 = extension_constructor_declaration
    { v3 :: v1 }
| v1 = str_extension_constructors BAR v3 = extension_constructor_rebind
    { v3 :: v1 }

sig_extension_constructors:
| v1 = extension_constructor_declaration
    { [v1] }
| v1 = sig_extension_constructors BAR v3 = extension_constructor_declaration
    { v3 :: v1 }

extension_constructor_declaration:
| v1 = constr_ident v2 = attributes v3 = generalized_constructor_arguments
    { let args, res = v3 in
      Te.decl (mkrhs $startpos(v1) $endpos(v1) v1) ~args ?res
              ~loc:(rloc $startpos $endpos) ~attrs:v2
    }

extension_constructor_rebind:
| v1 = constr_ident v2 = attributes EQUAL v4 = constr_longident
    { Te.rebind (mkrhs $startpos(v1) $endpos(v1) v1)
                (mkrhs $startpos(v4) $endpos(v4) v4)
                ~loc:(rloc $startpos $endpos) ~attrs:v2
    }

(* "with" constraints (additional type equations over signature components) *)

with_constraints:
| v1 = with_constraint
    { v1 }
| v1 = with_constraints AND v3 = with_constraint
    { v3 @ v1 }

with_constraint:
| TYPE v2 = type_parameters v3 = label_longident v4 = with_type_binder v5 = core_type v6 = constraints
    { [Pwith_type
          (mkrhs $startpos(v3) $endpos(v3) v3,
           (Type.mk (mkrhs $startpos(v3) $endpos(v3) (Longident.last v3))
              ~params:v2
              ~cstrs:(List.rev v6)
              ~manifest:v5
              ~priv:v4
              ~loc:(rloc $startpos $endpos)))] }
| TYPE v2 = type_parameters v3 = label COLONEQUAL v5 = core_type
    { [Pwith_typesubst
          (Type.mk (mkrhs $startpos(v3) $endpos(v3) v3)
             ~params:v2
             ~manifest:v5
             ~loc:(rloc $startpos $endpos))] }
| MODULE v2 = mod_longident EQUAL v4 = mod_ext_longident
    { [Pwith_module (mkrhs $startpos(v2) $endpos(v2) v2, mkrhs $startpos(v4) $endpos(v4) v4)] }
| MODULE v2 = UIDENT COLONEQUAL v4 = mod_ext_longident
    { [Pwith_modsubst (mkrhs $startpos(v2) $endpos(v2) v2, mkrhs $startpos(v4) $endpos(v4) v4)] }

with_type_binder:
| EQUAL
    { Public }
| EQUAL PRIVATE
    { Private }

(* Polymorphic types *)

typevar_list:
| QUOTE v2 = ident
    { [v2] }
| v1 = typevar_list QUOTE v3 = ident
    { v3 :: v1 }

poly_type:
| v1 = core_type
    { v1 }
| v1 = typevar_list DOT v3 = core_type
    { mktyp $startpos $endpos (Ptyp_poly(List.rev v1, v3)) }

(* Core types *)

core_type:
| v1 = core_type2
    { v1 }
| v1 = core_type2 AS QUOTE v4 = ident
    { mktyp $startpos $endpos (Ptyp_alias(v1, v4)) }

core_type2:
| v1 = simple_core_type_or_tuple
    { v1 }
| QUESTION v2 = LIDENT COLON v4 = core_type2 MINUSGREATER v6 = core_type2
    { mktyp $startpos $endpos (Ptyp_arrow("?" ^ v2 , mkoption v4, v6)) }
| v1 = OPTLABEL v2 = core_type2 MINUSGREATER v4 = core_type2
    { mktyp $startpos $endpos (Ptyp_arrow("?" ^ v1 , mkoption v2, v4)) }
| v1 = LIDENT COLON v3 = core_type2 MINUSGREATER v5 = core_type2
    { mktyp $startpos $endpos (Ptyp_arrow(v1, v3, v5)) }
| v1 = core_type2 MINUSGREATER v3 = core_type2
    { mktyp $startpos $endpos (Ptyp_arrow("", v1, v3)) }

simple_core_type:
| v1 = simple_core_type2 %prec below_SHARP
    { v1 }
| LPAREN v2 = core_type_comma_list RPAREN %prec below_SHARP
    { match v2 with [sty] -> sty | _ -> syntax_error $startpos $endpos; failwith "TODO" }
| v1 = simple_core_type v2 = attribute
    { Typ.attr v1 v2 }

simple_core_type_no_attr:
| v1 = simple_core_type2 %prec below_SHARP
    { v1 }
| LPAREN v2 = core_type_comma_list RPAREN %prec below_SHARP
    { match v2 with [sty] -> sty | _ -> syntax_error $startpos $endpos; failwith "TODO" }

simple_core_type2:
| QUOTE v2 = ident
    { mktyp $startpos $endpos (Ptyp_var v2) }
| UNDERSCORE
    { mktyp $startpos $endpos (Ptyp_any) }
| v1 = type_longident
    { mktyp $startpos $endpos (Ptyp_constr(mkrhs $startpos(v1) $endpos(v1) v1, [])) }
| v1 = simple_core_type2 v2 = type_longident
    { mktyp $startpos $endpos (Ptyp_constr(mkrhs $startpos(v2) $endpos(v2) v2, [v1])) }
| LPAREN v2 = core_type_comma_list RPAREN v4 = type_longident
    { mktyp $startpos $endpos (Ptyp_constr(mkrhs $startpos(v4) $endpos(v4) v4, List.rev v2)) }
| LESS v2 = meth_list GREATER
    { let (f, c) = v2 in mktyp $startpos $endpos (Ptyp_object (f, c)) }
| LESS GREATER
    { mktyp $startpos $endpos (Ptyp_object ([], Closed)) }
| SHARP v2 = class_longident
    { mktyp $startpos $endpos (Ptyp_class(mkrhs $startpos(v2) $endpos(v2) v2, [])) }
| v1 = simple_core_type2 SHARP v3 = class_longident
    { mktyp $startpos $endpos (Ptyp_class(mkrhs $startpos(v3) $endpos(v3) v3, [v1])) }
| LPAREN v2 = core_type_comma_list RPAREN SHARP v5 = class_longident
    { mktyp $startpos $endpos (Ptyp_class(mkrhs $startpos(v5) $endpos(v5) v5, List.rev v2)) }
| LBRACKET v2 = tag_field RBRACKET
    { mktyp $startpos $endpos (Ptyp_variant([v2], Closed, None)) }
(* PR#3835: this is not LR(1), would need lookahead=2
  | LBRACKET simple_core_type RBRACKET
      { mktyp $startpos $endpos (Ptyp_variant([$2], Closed, None)) }
*)
| LBRACKET BAR v3 = row_field_list RBRACKET
    { mktyp $startpos $endpos (Ptyp_variant(List.rev v3, Closed, None)) }
| LBRACKET v2 = row_field BAR v4 = row_field_list RBRACKET
    { mktyp $startpos $endpos (Ptyp_variant(v2 :: List.rev v4, Closed, None)) }
| LBRACKETGREATER opt_bar v3 = row_field_list RBRACKET
    { mktyp $startpos $endpos (Ptyp_variant(List.rev v3, Open, None)) }
| LBRACKETGREATER RBRACKET
    { mktyp $startpos $endpos (Ptyp_variant([], Open, None)) }
| LBRACKETLESS opt_bar v3 = row_field_list RBRACKET
    { mktyp $startpos $endpos (Ptyp_variant(List.rev v3, Closed, Some [])) }
| LBRACKETLESS opt_bar v3 = row_field_list GREATER v5 = name_tag_list RBRACKET
    { mktyp $startpos $endpos (Ptyp_variant(List.rev v3, Closed, Some (List.rev v5))) }
| LPAREN MODULE v3 = package_type RPAREN
    { mktyp $startpos $endpos (Ptyp_package v3) }
| v1 = extension
    { mktyp $startpos $endpos  (Ptyp_extension v1) }

package_type:
| v1 = mty_longident
    { (mkrhs $startpos(v1) $endpos(v1) v1, []) }
| v1 = mty_longident WITH v3 = package_type_cstrs
    { (mkrhs $startpos(v1) $endpos(v1) v1, v3) }

package_type_cstr:
| TYPE v2 = label_longident EQUAL v4 = core_type
    { (mkrhs $startpos(v2) $endpos(v2) v2, v4) }

package_type_cstrs:
| v1 = package_type_cstr
    { [v1] }
| v1 = package_type_cstr AND v3 = package_type_cstrs
    { v1::v3 }

row_field_list:
| v1 = row_field
    { [v1] }
| v1 = row_field_list BAR v3 = row_field
    { v3 :: v1 }

row_field:
| v1 = tag_field
    { v1 }
| v1 = simple_core_type
    { Rinherit v1 }

tag_field:
| v1 = name_tag v2 = attributes OF v4 = opt_ampersand v5 = amper_type_list
    { Rtag (v1, v2, v4, List.rev v5) }
| v1 = name_tag v2 = attributes
    { Rtag (v1, v2, true, []) }

opt_ampersand:
| AMPERSAND
    { true }
| (* empty *)
    { false }

amper_type_list:
| v1 = core_type
    { [v1] }
| v1 = amper_type_list AMPERSAND v3 = core_type
    { v3 :: v1 }

name_tag_list:
| v1 = name_tag
    { [v1] }
| v1 = name_tag_list v2 = name_tag
    { v2 :: v1 }

simple_core_type_or_tuple:
| v1 = simple_core_type %prec below_LBRACKETAT
    { v1 }
| v1 = simple_core_type STAR v3 = core_type_list
    { mktyp $startpos $endpos (Ptyp_tuple(v1 :: List.rev v3)) }

simple_core_type_or_tuple_no_attr:
| v1 = simple_core_type_no_attr
    { v1 }
| v1 = simple_core_type_no_attr STAR v3 = core_type_list_no_attr
    { mktyp $startpos $endpos (Ptyp_tuple(v1 :: List.rev v3)) }

core_type_comma_list:
| v1 = core_type
    { [v1] }
| v1 = core_type_comma_list COMMA v3 = core_type
    { v3 :: v1 }

core_type_list:
| v1 = simple_core_type %prec below_LBRACKETAT
    { [v1] }
| v1 = core_type_list STAR v3 = simple_core_type
    { v3 :: v1 }

core_type_list_no_attr:
| v1 = simple_core_type_no_attr
    { [v1] }
| v1 = core_type_list STAR v3 = simple_core_type_no_attr
    { v3 :: v1 }

meth_list:
| v1 = field SEMI v3 = meth_list
    { let (f, c) = v3 in (v1 :: f, c) }
| v1 = field opt_semi
    { [v1], Closed }
| DOTDOT
    { [], Open }

field:
| v1 = label v2 = attributes COLON v4 = poly_type
    { (v1, v2, v4) }

label:
| v1 = LIDENT
    { v1 }

(* Constants *)

constant:
| v1 = INT
    { Const_int v1 }
| v1 = CHAR
    { Const_char v1 }
| v1 = STRING
    { let (s, d) = v1 in Const_string (s, d) }
| v1 = FLOAT
    { Const_float v1 }
| v1 = INT32
    { Const_int32 v1 }
| v1 = INT64
    { Const_int64 v1 }
| v1 = NATIVEINT
    { Const_nativeint v1 }

signed_constant:
| v1 = constant
    { v1 }
| MINUS v2 = INT
    { Const_int(- v2) }
| MINUS v2 = FLOAT
    { Const_float("-" ^ v2) }
| MINUS v2 = INT32
    { Const_int32(Int32.neg v2) }
| MINUS v2 = INT64
    { Const_int64(Int64.neg v2) }
| MINUS v2 = NATIVEINT
    { Const_nativeint(Nativeint.neg v2) }
| PLUS v2 = INT
    { Const_int v2 }
| PLUS v2 = FLOAT
    { Const_float v2 }
| PLUS v2 = INT32
    { Const_int32 v2 }
| PLUS v2 = INT64
    { Const_int64 v2 }
| PLUS v2 = NATIVEINT
    { Const_nativeint v2 }

(* Identifiers and long identifiers *)

ident:
| v1 = UIDENT
    { v1 }
| v1 = LIDENT
    { v1 }

val_ident:
| v1 = LIDENT
    { v1 }
| LPAREN @{`Unclosed "("} v2 = operator RPAREN
    { v2 }
(*| LPAREN v2 =error
    { expecting $startpos(v2) $endpos(v2) "operator" }*)
(*| LPAREN MODULE v3 = error
    { expecting $startpos(v3) $endpos(v3) "module-expr" }*)

operator:
| v1 = PREFIXOP
    { v1 }
| v1 = INFIXOP0
    { v1 }
| v1 = INFIXOP1
    { v1 }
| v1 = INFIXOP2
    { v1 }
| v1 = INFIXOP3
    { v1 }
| v1 = INFIXOP4
    { v1 }
| BANG
    { "!" }
| PLUS
    { "+" }
| PLUSDOT
    { "+." }
| MINUS
    { "-" }
| MINUSDOT
    { "-." }
| STAR
    { "*" }
| EQUAL
    { "=" }
| LESS
    { "<" }
| GREATER
    { ">" }
| OR
    { "or" }
| BARBAR
    { "||" }
| AMPERSAND
    { "&" }
| AMPERAMPER
    { "&&" }
| COLONEQUAL
    { ":=" }
| PLUSEQ
    { "+=" }
| PERCENT
    { "%" }

constr_ident:
| v1 = UIDENT
    { v1 }
(*  | LBRACKET RBRACKET                           { "[]" } *)
| LPAREN RPAREN
    { "()" }
| COLONCOLON
    { "::" }
(*  | LPAREN COLONCOLON RPAREN                    { "::" } *)
| FALSE
    { "false" }
| TRUE
    { "true" }

val_longident:
| v1 = val_ident
    { Lident v1 }
| v1 = mod_longident DOT v3 = val_ident
    { Ldot(v1, v3) }

constr_longident:
| v1 = mod_longident %prec below_DOT
    { v1 }
| LBRACKET RBRACKET
    { Lident "[]" }
| LPAREN RPAREN
    { Lident "()" }
| FALSE
    { Lident "false" }
| TRUE
    { Lident "true" }

label_longident:
| v1 = LIDENT
    { Lident v1 }
| v1 = mod_longident DOT v3 = LIDENT
    { Ldot(v1, v3) }

type_longident:
| v1 = LIDENT
    { Lident v1 }
| v1 = mod_ext_longident DOT v3 = LIDENT
    { Ldot(v1, v3) }

mod_longident:
| v1 = UIDENT
    { Lident v1 }
| v1 = mod_longident DOT v3 = UIDENT
    { Ldot(v1, v3) }

mod_ext_longident:
| v1 = UIDENT
    { Lident v1 }
| v1 = mod_ext_longident DOT v3 = UIDENT
    { Ldot(v1, v3) }
| v1 = mod_ext_longident LPAREN v3 = mod_ext_longident RPAREN
    { lapply $startpos $endpos v1 v3 }

mty_longident:
| v1 = ident
    { Lident v1 }
| v1 = mod_ext_longident DOT v3 = ident
    { Ldot(v1, v3) }

clty_longident:
| v1 = LIDENT
    { Lident v1 }
| v1 = mod_ext_longident DOT v3 = LIDENT
    { Ldot(v1, v3) }

class_longident:
| v1 = LIDENT
    { Lident v1 }
| v1 = mod_longident DOT v3 = LIDENT
    { Ldot(v1, v3) }

(* Miscellaneous *)

name_tag:
| BACKQUOTE v2 = ident
    { v2 }

rec_flag:
| (* empty *)
    { Nonrecursive }
| REC
    { Recursive }

direction_flag:
| TO
    { Upto }
| DOWNTO
    { Downto }

private_flag:
| (* empty *)
    { Public }
| PRIVATE
    { Private }

mutable_flag:
| (* empty *)
    { Immutable }
| MUTABLE
    { Mutable }

virtual_flag:
| (* empty *)
    { Concrete }
| VIRTUAL
    { Virtual }

private_virtual_flags:
| (* empty *)
    { Public, Concrete }
| PRIVATE
    { Private, Concrete }
| VIRTUAL
    { Public, Virtual }
| PRIVATE VIRTUAL
    { Private, Virtual }
| VIRTUAL PRIVATE
    { Private, Virtual }

override_flag:
| (* empty *)
    { Fresh }
| BANG
    { Override }

opt_bar:
| (* empty *)
    { () }
| BAR
    { () }

opt_semi:
| (* empty *)
    { () }
| SEMI
    { () }

subtractive:
| MINUS
    { "-" }
| MINUSDOT
    { "-." }

additive:
| PLUS
    { "+" }
| PLUSDOT
    { "+." }


(* Attributes and extensions *)

single_attr_id:
| v1 = LIDENT
    { v1 }
| v1 = UIDENT
    { v1 }
| AND
    { "and" }
| AS
    { "as" }
| ASSERT
    { "assert" }
| BEGIN
    { "begin" }
| CLASS
    { "class" }
| CONSTRAINT
    { "constraint" }
| DO
    { "do" }
| DONE
    { "done" }
| DOWNTO
    { "downto" }
| ELSE
    { "else" }
| END
    { "end" }
| EXCEPTION
    { "exception" }
| EXTERNAL
    { "external" }
| FALSE
    { "false" }
| FOR
    { "for" }
| FUN
    { "fun" }
| FUNCTION
    { "function" }
| FUNCTOR
    { "functor" }
| IF
    { "if" }
| IN
    { "in" }
| INCLUDE
    { "include" }
| INHERIT
    { "inherit" }
| INITIALIZER
    { "initializer" }
| LAZY
    { "lazy" }
| LET
    { "let" }
| MATCH
    { "match" }
| METHOD
    { "method" }
| MODULE
    { "module" }
| MUTABLE
    { "mutable" }
| NEW
    { "new" }
| OBJECT
    { "object" }
| OF
    { "of" }
| OPEN
    { "open" }
| OR
    { "or" }
| PRIVATE
    { "private" }
| REC
    { "rec" }
| SIG
    { "sig" }
| STRUCT
    { "struct" }
| THEN
    { "then" }
| TO
    { "to" }
| TRUE
    { "true" }
| TRY
    { "try" }
| TYPE
    { "type" }
| VAL
    { "val" }
| VIRTUAL
    { "virtual" }
| WHEN
    { "when" }
| WHILE
    { "while" }
| WITH
    { "with" }
(* mod/land/lor/lxor/lsl/lsr/asr are not supported for now *)

attr_id:
| v1 = single_attr_id
    { mkloc v1 (rloc $startpos $endpos) }
| v1 = single_attr_id DOT v3 = attr_id
    { mkloc (v1 ^ "." ^ v3.txt) (rloc $startpos $endpos)}

attribute:
| LBRACKETAT v2 = attr_id v3 = payload RBRACKET
    { (v2, v3) }

post_item_attribute:
| LBRACKETATAT v2 = attr_id v3 = payload RBRACKET
    { (v2, v3) }

floating_attribute:
| LBRACKETATATAT @{`Item "attribute"}
  v2 = attr_id v3 = payload RBRACKET
    { (v2, v3) }

post_item_attributes:
| (* empty *)
    { [] }
| v1 = post_item_attribute v2 = post_item_attributes
    { v1 :: v2 }

attributes:
| (* empty *)
    { [] }
| v1 = attribute v2 = attributes
    { v1 :: v2 }

ext_attributes:
| (* empty *)
    { None, [] }
| v1 = attribute v2 = attributes
    { None, v1 :: v2 }
| PERCENT v2 = attr_id v3 = attributes
    { Some v2, v3 }

extension:
| LBRACKETPERCENT v2 = attr_id v3 = payload RBRACKET
    { (v2, v3) }

item_extension:
| LBRACKETPERCENTPERCENT @{`Item "extension"}
  v2 = attr_id v3 = payload RBRACKET
    { (v2, v3) }

payload:
| v1 = structure
    { PStr v1 }
| COLON v2 = core_type
    { PTyp v2 }
| QUESTION v2 = pattern
    { PPat (v2, None) }
| QUESTION v2 = pattern WHEN v4 = seq_expr
    { PPat (v2, Some v4) }

(* Merlin refactoring *)

newtype:
| LPAREN TYPE v3 = LIDENT RPAREN
    { v3 }

expr_open:
| v1 = override_flag v2 = ext_attributes v3 = mod_longident
    { v1, mkrhs $startpos(v3) $endpos(v3) v3, v2 }

(* Caml p4 extensions *)
structure_item:
| LET_LWT @{`Item "lwt"} ext_attributes rec_flag let_bindings
    { match $4 with
    | [ {pvb_pat = { ppat_desc = Ppat_any; ppat_loc = _ };
         pvb_expr = exp; pvb_attributes = attrs} ] ->
        let exp = wrap_exp_attrs $startpos $endpos exp $2 in
        mkstr $startpos $endpos (Pstr_eval (Fake.app Fake.Lwt.un_lwt exp, attrs))
    | _ ->
      let str = mkstr $startpos $endpos
            (Pstr_value ($3, List.rev_map (fake_vb_app Fake.Lwt.un_lwt) $4))
      in
      let (ext, attrs) = $2 in
      if attrs <> [] then not_expecting $startpos($2) $endpos($2) "attribute";
      match ext with
      | None -> str
      | Some id -> ghstr $startpos $endpos (Pstr_extension((id, PStr str), []))
    }
| TYPE NONREC @{`Item "type nonrec"} decls = type_declarations
    { let ty = List.map fake_tydecl decls in
      mkstr $startpos $endpos (Pstr_type(List.rev_map tag_nonrec ty)) }
| TYPE @{`Item "type"} type_declarations WITH with_extensions
    {
      let ghost_loc = Some (gloc $startpos($4) $endpos($4)) in
      let ty = List.map fake_tydecl $2 in
      let ast = Fake.TypeWith.generate_definitions ~ty ?ghost_loc $4 in
      mkstr $startpos $endpos (Pstr_type(List.rev $2)) @ ast
    }
| TYPE NONREC @{`Item "type nonrec"} type_declarations WITH with_extensions
    {
      let ghost_loc = Some (gloc $startpos($5) $endpos($5)) in
      let ty = List.map fake_tydecl $3 in
      let ast = Fake.TypeWith.generate_definitions ~ty ?ghost_loc $5 in
      mkstr $startpos $endpos (Pstr_type(List.rev_map tag_nonrec ty)) @ ast
    }
| OUNIT_TEST option(STRING) EQUAL seq_expr
    { let expr = Fake.app Fake.OUnit.force_bool $4 in
      mkstr $startpos $endpos (Pstr_eval (expr,[]))
    }
| OUNIT_TEST_UNIT option(STRING) EQUAL seq_expr
    { let expr = Fake.app Fake.OUnit.force_unit $4 in
      mkstr $startpos $endpos (Pstr_eval (expr,[]))
    }
| OUNIT_TEST_MODULE option(STRING) EQUAL module_expr
    { let name = Fake.OUnit.fresh_test_module_ident () in
      mkstr $startpos $endpos
         (Pstr_module(Mb.mk (mkrhs $startpos($1) $endpos($2) name) $4))
    }
| OUNIT_BENCH STRING EQUAL seq_expr
    { let expr = $4 in
      mkstr $startpos $endpos (Pstr_eval (expr,[]))
    }
| OUNIT_BENCH_FUN STRING EQUAL seq_expr
    { let expr = Fake.app Fake.OUnit.force_unit_arrow_unit $4 in
      mkstr $startpos $endpos (Pstr_eval (expr,[]))
    }
| OUNIT_BENCH_INDEXED STRING val_ident simple_expr EQUAL seq_expr
    { let f_arg = mkpat $startpos $endpos
                      (Ppat_var (mkrhs $startpos($3) $endpos($3) $3))
      in
      let f_fun = mkexp $startpos $endpos
          (Pexp_fun("", None, f_arg, $6))
      in
      let expr = Fake.(app (app OUnit.force_indexed f_fun) $4) in
      mkstr $startpos $endpos (Pstr_eval (expr,[]))
    }
| OUNIT_BENCH_MODULE STRING EQUAL module_expr
    { let name = Fake.OUnit.fresh_test_module_ident () in
      mkstr $startpos $endpos
         (Pstr_module(Mb.mk (mkrhs $startpos($1) $endpos($2) name) $4))
    }
;

signature_item:
| TYPE NONREC @{`Item "type nonrec"} decls = type_declarations
    { let ty = List.map fake_tydecl decls in
      mksig $startpos $endpos (Psig_type (List.rev_map tag_nonrec ty)) }
| TYPE @{`Item "type"} type_declarations WITH with_extensions
    {
      let ghost_loc = Some (gloc $startpos($4) $endpos($4)) in
      let ty = List.map fake_tydecl $2 in
      let decls = Fake.TypeWith.generate_sigs ~ty ?ghost_loc $4 in
      List.rev_append decls (mksig $startpos $endpos (Psig_type(List.rev $2)))
    }
| TYPE NONREC @{`Item "type nonrec"} type_declarations WITH with_extensions
    {
      let ghost_loc = Some (gloc $startpos($5) $endpos($5)) in
      let ty = List.map fake_tydecl $3 in
      let decls = Fake.TypeWith.generate_sigs ~ty ?ghost_loc $5 in
      List.rev_append decls (mksig $startpos $endpos
            (Psig_type(List.rev_map tag_nonrec ty)))
    }

with_extensions:
| LIDENT COMMA with_extensions { $1 :: $3 }
| LIDENT { [$1] }


expr:
| LET_LWT @{`Item "lwt"}
  ext_attributes rec_flag let_bindings IN @{`Shift 2} seq_expr
    { let expr = reloc_exp_fake $endpos($5) $endpos $6 in
      let expr = Pexp_let($3, List.rev_map (fake_vb_app Fake.Lwt.un_lwt) $4, expr) in
      Fake.app Fake.Lwt.in_lwt (mkexp_attrs $startpos $endpos expr $2) }
| MATCH_LWT @{`Item "match_lwt"}
  ext_attributes seq_expr WITH opt_bar match_cases
    { let expr = mkexp_attrs $startpos $endpos
          (Pexp_match(Fake.app Fake.Lwt.un_lwt $3, List.rev $6)) $2 in
      Fake.app Fake.Lwt.in_lwt expr }
| TRY_LWT @{`Item "try_lwt"}
  ext_attributes seq_expr %prec below_WITH
    { reloc_exp $startpos $endpos (Fake.app Fake.Lwt.in_lwt $3) }
| TRY_LWT @{`Item "try_lwt"}
  ext_attributes seq_expr WITH opt_bar match_cases
    { mkexp_attrs $startpos $endpos
        (Pexp_try(Fake.app Fake.Lwt.in_lwt $3, List.rev $6)) $2 }
| TRY_LWT @{`Item "try_lwt"}
  ext_attributes seq_expr FINALLY_LWT seq_expr
    { Fake.app (Fake.app Fake.Lwt.finally' $3) $5 }
| TRY_LWT @{`Item "try_lwt"}
  ext_attributes seq_expr WITH opt_bar match_cases FINALLY_LWT seq_expr
    { let expr = mkexp_attrs $startpos $endpos
        (Pexp_try (Fake.app Fake.Lwt.in_lwt $3, List.rev $6)) $2 in
      Fake.app (Fake.app Fake.Lwt.finally' expr) $8 }
| WHILE_LWT @{`Item "while_lwt"}
  ext_attributes seq_expr
  DO @{`Item "while_lwt body"} seq_expr DONE
  { let expr = Pexp_while ($3, Fake.(app Lwt.un_lwt $5)) in
    Fake.(app Lwt.to_lwt (mkexp_attrs $startpos $endpos expr $2)) }
| FOR_LWT @{`Item "for_lwt"}
  ext_attributes pattern EQUAL seq_expr direction_flag seq_expr
  DO @{`Item "for body"} seq_expr DONE
    { let expr = Pexp_for ($3, $5, $7, $6, Fake.(app Lwt.un_lwt $9)) in
      Fake.(app Lwt.to_lwt (mkexp_attrs $startpos $endpos expr $2)) }
| FOR_LWT @{`Item "for_lwt"}
  ext_attributes pattern IN seq_expr
  DO @{`Item "for body"} seq_expr DONE
    { mkexp_attrs $startpos $endpos
          (Pexp_let (Nonrecursive, [Vb.mk $3 (Fake.(app Lwt.un_stream $5))],
             Fake.(app Lwt.unit_lwt $7)))
          $2
    }
;

expr:
| simple_expr SHARP SHARP label LESSMINUS expr
    { let inst = Fake.(app Js.un_js $1) in
      let field = mkexp $startpos $endpos($4) (Pexp_send(inst, $4)) in
      let prop = Fake.(app Js.un_prop field) in
      let setter = mkexp $startpos $endpos($4) (Pexp_send(prop,"set")) in
      reloc_exp $startpos $endpos
      Fake.(app setter $6)
    }
;

simple_expr:
| simple_expr SHARP SHARP @{`Shift_token (1,LIDENT "")} label
    { let inst = Fake.(app Js.un_js $1) in
      let field = mkexp $startpos $endpos (Pexp_send(inst, $4)) in
      let prop = Fake.(app Js.un_prop field) in
      mkexp $startpos $endpos (Pexp_send(prop,"get"))
    }
| simple_expr SHARP SHARP label LPAREN RPAREN
    { let inst = Fake.(app Js.un_js $1) in
      let jsmeth = mkexp $startpos $endpos($4) (Pexp_send(inst, $4)) in
      Fake.(app Js.un_meth jsmeth)
    }
| simple_expr SHARP SHARP label LPAREN expr_comma_opt_list RPAREN
    { let inst = Fake.(app Js.un_js $1) in
      let meth = mkexp $startpos $endpos($4) (Pexp_send(inst, $4)) in
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

simple_expr:
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

expr_comma_opt_list:
    expr_comma_opt_list COMMA expr              { $3 :: $1 }
  | expr %prec COMMA                            { [$1] }
;

%%
