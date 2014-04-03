%{
open Location
open Asttypes
open Longident
open Parsetree
open Ast_helper


let rloc loc_start loc_end =
  { loc_start; loc_end; loc_ghost = false; }
let gloc loc_start loc_end =
  { loc_start; loc_end; loc_ghost = true; }

let mktyp startpos endpos d   = Typ.mk ~loc:(rloc startpos endpos) d
let mkpat startpos endpos d   = Pat.mk ~loc:(rloc startpos endpos) d
let mkexp startpos endpos d   = Exp.mk ~loc:(rloc startpos endpos) d
let mkmty startpos endpos d   = Mty.mk ~loc:(rloc startpos endpos) d
let mksig startpos endpos d   = Sig.mk ~loc:(rloc startpos endpos) d
let mkmod startpos endpos d   = Mod.mk ~loc:(rloc startpos endpos) d
let mkstr startpos endpos d   = Str.mk ~loc:(rloc startpos endpos) d
let mkclass startpos endpos d = Cl.mk  ~loc:(rloc startpos endpos) d
let mkcty startpos endpos d   = Cty.mk ~loc:(rloc startpos endpos) d
let mkctf startpos endpos d   = Ctf.mk ~loc:(rloc startpos endpos) d
let mkcf  startpos endpos d   = Cf.mk  ~loc:(rloc startpos endpos) d

let mkrhs startpos endpos rhs = mkloc rhs (rloc startpos endpos)
let mkoption d =
  let loc = {d.ptyp_loc with loc_ghost = true} in
  Typ.mk ~loc (Ptyp_constr(mkloc (Ldot (Lident "*predef*", "option")) loc,[d]))

let reloc_pat startpos endpos x= { x with ppat_loc = rloc startpos endpos };;
let reloc_exp startpos endpos x= { x with pexp_loc = rloc startpos endpos };;

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
      mkexp startpos endpos (Pexp_apply(mkoperator ("~" ^ name) 1, ["", arg]))

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
      let pat_pl = mktailpat p1.ppat_loc.loc_end endpos pl in
      let loc = gloc p1.ppat_loc.loc_start pat_pl.ppat_loc.loc_end in
      let arg = Pat.mk ~loc (Ppat_tuple [p1; pat_pl]) in
      mkpat_cons loc arg loc

let mkstrexp e attrs =
  { pstr_desc = Pstr_eval (e, attrs); pstr_loc = e.pexp_loc }

let mkexp_constraint e (t1, t2) =
  match t1, t2 with
  | Some t, None -> ghexp(Pexp_constraint(e, t))
  | _, Some t -> ghexp(Pexp_coerce(e, t1, t))
  | None, None -> assert false

let array_function startpos endpos str name =
  ghloc startpos endpos
    (Ldot(Lident str, (if !Clflags.fast then "unsafe_" ^ name else name)))

let syntax_error pos =
  Parsing_aux.raise_warning (Syntaxerr.Escape_error pos)

let unclosed opening_name opstart opend closing_name clstart clend =
  Parsing_aux.raise_warning
    Syntaxerr.(Error (Unclosed (rloc opstart opend, opening_name,
                                rloc clstart clend, closing_name)))

let expecting startpos endpos nonterm =
  Parsing_aux.raise_warning
    Syntaxerr.(Error (Expecting (rloc startpos endpos, nonterm)))

let not_expecting startpos endpos nonterm =
  Parsing_aux.raise_warning
    Syntaxerr.(Error (Not_expecting (rloc startpos endpos, nonterm)))

let bigarray_function startpos endpos str name =
  ghloc startpos endpos (Ldot(Ldot(Lident "Bigarray", str), name))

let bigarray_untuplify = function
    { pexp_desc = Pexp_tuple explist; pexp_loc = _ } -> explist
  | exp -> [exp]

let bigarray_get startpos endpos arr arg =
  let get = if Clflags.fast () then "unsafe_get" else "get" in
  let ghexp = ghexp startpos endpos in
  let bigarray_function = bigarray_function startpos endpos in
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

let bigarray_set startpos endpos arr arg newval =
  let set = if !Clflags.fast then "unsafe_set" else "set" in
  let ghexp = ghexp startpos endpos in
  let bigarray_function = bigarray_function startpos endpos in
  let mkexp = mkexp startpos endpos in
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
  if !Clflags.applicative_functors
  then Lapply(p1, p2)
  else raise (Syntaxerr.Error(Syntaxerr.Applicative_path (rloc startpos endpos)))

let exp_of_label startpos endpos lbl =
  mkexp (Pexp_ident(mkrhs startpos endpos (Lident(Longident.last lbl))))

let pat_of_label startpos endpos lbl =
  mkpat (Ppat_var (mkrhs startpos endpos (Longident.last lbl)))

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
          Ptyp_object (List.map (fun (s, t) -> (s, loop t)) lst, o)
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
      | Rtag(label,flag,lst) ->
          Rtag(label,flag,List.map loop lst)
      | Rinherit t ->
          Rinherit (loop t)
  in
  loop t

let wrap_type_annotation startpos endpos newtypes core_type body =
  let mkexp = mkexp startpos endpos in
  let exp = mkexp(Pexp_constraint(body,core_type)) in
  let exp =
    List.fold_right (fun newtype exp -> mkexp (Pexp_newtype (newtype, exp)))
      newtypes exp
  in
  (exp, ghtyp(Ptyp_poly(newtypes,varify_constructors newtypes core_type)))

let wrap_exp_attrs startpos endpos body (ext, attrs) =
  (* todo: keep exact location for the entire attribute *)
  let body = {body with pexp_attributes = attrs @ body.pexp_attributes} in
  match ext with
  | None -> body
  | Some id -> ghexp startpos endpos (Pexp_extension (id, PStr [mkstrexp body []]))

let mkexp_attrs startpos endpos  d attrs =
  wrap_exp_attrs startpos endpos (mkexp d) attrs

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
%token LBRACKETAT
%token LBRACKETATAT
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
%nonassoc LET                           (* above SEMI ( ...; let ... in ...) *)
%nonassoc below_WITH
%nonassoc FUNCTION WITH                 (* below BAR  (match ... with ...) *)
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
%nonassoc LBRACKETPERCENT
%nonassoc LBRACKETPERCENTPERCENT
%right    COLONCOLON                    (* expr (e :: e :: e) *)
%left     INFIXOP2 PLUS PLUSDOT MINUS MINUSDOT  (* expr (e OP e OP e) *)
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

%%

(* Entry points *)

implementation:
| v1 = structure EOF
    { v1 }

interface:
| v1 = signature EOF
    { v1 }

toplevel_phrase:
| v1 = top_structure SEMISEMI
    { Ptop_def v1 }
| v1 = toplevel_directive SEMISEMI
    { v1 }
| EOF
    { raise End_of_file }

top_structure:
| v1 = str_attribute v2 = top_structure
    { v1 :: v2 }
| v1 = seq_expr v2 = post_item_attributes
    { [mkstrexp v1 v2] }
| v1 = top_structure_tail
    { v1 }

top_structure_tail:
| (* empty *)
    { [] }
| v1 = structure_item v2 = top_structure_tail
    { v1 :: v2 }

use_file:
| v1 = use_file_tail
    { v1 }
| v1 = seq_expr v2 = post_item_attributes v3 = use_file_tail
    { Ptop_def[mkstrexp v1 v2] :: v3 }

use_file_tail:
| EOF
    { [] }
| SEMISEMI EOF
    { [] }
| SEMISEMI v2 = seq_expr v3 = post_item_attributes v4 = use_file_tail
    { Ptop_def[mkstrexp v2 v3] :: v4 }
| SEMISEMI v2 = structure_item v3 = use_file_tail
    { Ptop_def[v2] :: v3 }
| SEMISEMI v2 = toplevel_directive v3 = use_file_tail
    { v2 :: v3 }
| v1 = structure_item v2 = use_file_tail
    { Ptop_def[v1] :: v2 }
| v1 = toplevel_directive v2 = use_file_tail
    { v1 :: v2 }

parse_core_type:
| v1 = core_type EOF
    { v1 }

parse_expression:
| v1 = seq_expr EOF
    { v1 }

parse_pattern:
| v1 = pattern EOF
    { v1 }

(* Module expressions *)

functor_arg:
| LPAREN RPAREN
    { mkrhs $startpos(v2) $endpos(v2) "()", None }
| LPAREN v2 = functor_arg_name COLON v4 = module_type RPAREN
    { mkrhs v2 2, Some v4 }

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
    { mkmod $startpos $endpos (Pmod_ident (mkrhs v1 1)) }
| STRUCT v2 = structure END
    { mkmod $startpos $endpos (Pmod_structure(v2)) }
| STRUCT structure error
    { unclosed "struct" 1 "end" 3 }
| FUNCTOR v2 = functor_args MINUSGREATER v4 = module_expr
    { List.fold_left (fun acc (n, t) -> mkmod $startpos $endpos (Pmod_functor(n, t, acc))) v4 v2 }
| v1 = module_expr LPAREN v3 = module_expr RPAREN
    { mkmod $startpos $endpos (Pmod_apply(v1, v3)) }
| v1 = module_expr LPAREN RPAREN
    { mkmod $startpos $endpos (Pmod_apply(v1, mkmod (Pmod_structure []))) }
| module_expr LPAREN module_expr error
    { unclosed "(" 2 ")" 4 }
| LPAREN v2 = module_expr COLON v4 = module_type RPAREN
    { mkmod $startpos $endpos (Pmod_constraint(v2, v4)) }
| LPAREN module_expr COLON module_type error
    { unclosed "(" 1 ")" 5 }
| LPAREN v2 = module_expr RPAREN
    { v2 }
| LPAREN module_expr error
    { unclosed "(" 1 ")" 3 }
| LPAREN VAL v3 = expr RPAREN
    { mkmod $startpos $endpos (Pmod_unpack v3) }
| LPAREN VAL v3 = expr COLON v5 = package_type RPAREN
    { mkmod $startpos $endpos (Pmod_unpack(
              ghexp(Pexp_constraint(v3, ghtyp(Ptyp_package v5))))) }
| LPAREN VAL v3 = expr COLON v5 = package_type COLONGREATER v7 = package_type RPAREN
    { mkmod $startpos $endpos (Pmod_unpack(
              ghexp(Pexp_coerce(v3, Some(ghtyp(Ptyp_package v5)),
                                    ghtyp(Ptyp_package v7))))) }
| LPAREN VAL v3 = expr COLONGREATER v5 = package_type RPAREN
    { mkmod $startpos $endpos (Pmod_unpack(
              ghexp(Pexp_coerce(v3, None, ghtyp(Ptyp_package v5))))) }
| LPAREN VAL expr COLON error
    { unclosed "(" 1 ")" 5 }
| LPAREN VAL expr COLONGREATER error
    { unclosed "(" 1 ")" 5 }
| LPAREN VAL expr error
    { unclosed "(" 1 ")" 4 }
| v1 = module_expr v2 = attribute
    { Mod.attr v1 v2 }
| v1 = extension
    { mkmod $startpos $endpos (Pmod_extension v1) }

structure:
| v1 = str_attribute v2 = structure
    { v1 :: v2 }
| v1 = seq_expr v2 = post_item_attributes v3 = structure_tail
    { mkstrexp v1 v2 :: v3 }
| v1 = structure_tail
    { v1 }

structure_tail:
| (* empty *)
    { [] }
| SEMISEMI v2 = structure
    { v2 }
| v1 = structure_item v2 = structure_tail
    { v1 :: v2 }

str_attribute:
| v1 = post_item_attribute
    { mkstr $startpos $endpos (Pstr_attribute v1) }

structure_item:
| LET v2 = ext_attributes v3 = rec_flag v4 = let_bindings
    {
        match v4 with
          [ {pvb_pat = { ppat_desc = Ppat_any; ppat_loc = _ };
             pvb_expr = exp; pvb_attributes = attrs}] ->
            let exp = wrap_exp_attrs exp v2 in
            mkstr $startpos $endpos (Pstr_eval (exp, attrs))
        | l ->
            begin match v2 with
            | None, [] -> mkstr $startpos $endpos (Pstr_value(v3, List.rev l))
            | Some _, _ -> not_expecting 2 "extension"
            | None, _ :: _ -> not_expecting 2 "attribute"
            end
      }
| EXTERNAL v2 = val_ident COLON v4 = core_type EQUAL v6 = primitive_declaration v7 = post_item_attributes
    { mkstr $startpos $endpos
          (Pstr_primitive (Val.mk (mkrhs $startpos(v2) $endpos(v2) v2) v4
                             ~prim:v6 ~attrs:v7 ~loc:(rloc $startpos $endpos))) }
| TYPE v2 = type_declarations
    { mkstr $startpos $endpos (Pstr_type (List.rev v2) ) }
| EXCEPTION v2 = exception_declaration
    { mkstr $startpos $endpos (Pstr_exception v2) }
| EXCEPTION v2 = UIDENT EQUAL v4 = constr_longident v5 = post_item_attributes
    { mkstr $startpos $endpos (Pstr_exn_rebind(mkrhs $startpos(v2) $endpos(v2) v2, mkloc v4 (rloc $startpos(v4) $endpos(v4)), v5)) }
| MODULE v2 = module_binding
    { mkstr $startpos $endpos (Pstr_module v2) }
| MODULE REC v3 = module_bindings
    { mkstr $startpos $endpos (Pstr_recmodule(List.rev v3)) }
| MODULE TYPE v3 = ident v4 = post_item_attributes
    { mkstr $startpos $endpos (Pstr_modtype (Mtd.mk (mkrhs $startpos(v3) $endpos(v3) v3)
                              ~attrs:v4 ~loc:(rloc $startpos $endpos))) }
| MODULE TYPE v3 = ident EQUAL v5 = module_type v6 = post_item_attributes
    { mkstr $startpos $endpos (Pstr_modtype (Mtd.mk (mkrhs $startpos(v3) $endpos(v3) v3)
                              ~typ:v5 ~attrs:v6 ~loc:(rloc $startpos $endpos))) }
| OPEN v2 = override_flag v3 = mod_longident v4 = post_item_attributes
    { mkstr $startpos $endpos (Pstr_open (v2, mkrhs $startpos(v3) $endpos(v3) v3, v4)) }
| CLASS v2 = class_declarations
    { mkstr $startpos $endpos (Pstr_class (List.rev v2)) }
| CLASS TYPE v3 = class_type_declarations
    { mkstr $startpos $endpos (Pstr_class_type (List.rev v3)) }
| INCLUDE v2 = module_expr v3 = post_item_attributes
    { mkstr $startpos $endpos (Pstr_include (v2, v3)) }
| v1 = item_extension v2 = post_item_attributes
    { mkstr $startpos $endpos (Pstr_extension (v1, v2)) }

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
| SIG v2 = signature END
    { mkmty $startpos $endpos (Pmty_signature v2) }
| SIG signature error
    { unclosed "sig" 1 "end" 3 }
| FUNCTOR v2 = functor_args MINUSGREATER v4 = module_type %prec below_WITH
    { List.fold_left (fun acc (n, t) -> mkmty $startpos $endpos (Pmty_functor(n, t, acc))) v4 v2 }
| v1 = module_type WITH v3 = with_constraints
    { mkmty $startpos $endpos (Pmty_with(v1, List.rev v3)) }
| MODULE TYPE OF v4 = module_expr %prec below_LBRACKETAT
    { mkmty $startpos $endpos (Pmty_typeof v4) }
| LPAREN MODULE v3 = mod_longident RPAREN
    { mkmty $startpos $endpos  (Pmty_alias (mkrhs $startpos(v3) $endpos(v3) v3)) }
| LPAREN v2 = module_type RPAREN
    { v2 }
| LPAREN module_type error
    { unclosed "(" 1 ")" 3 }
| v1 = extension
    { mkmty $startpos $endpos (Pmty_extension v1) }
| v1 = module_type v2 = attribute
    { Mty.attr v1 v2 }

signature:
| v1 = sig_attribute v2 = signature
    { v1 :: v2 }
| v1 = signature_tail
    { v1 }

signature_tail:
| (* empty *)
    { [] }
| SEMISEMI v2 = signature
    { v2 }
| v1 = signature_item v2 = signature_tail
    { v1 :: v2 }

sig_attribute:
| v1 = post_item_attribute
    { mksig $startpos $endpos (Psig_attribute v1) }

signature_item:
| VAL v2 = val_ident COLON v4 = core_type v5 = post_item_attributes
    { mksig $startpos $endpos (Psig_value
                (Val.mk (mkrhs $startpos(v2) $endpos(v2) v2) v4 ~attrs:v5 ~loc:(rloc $startpos $endpos))) }
| EXTERNAL v2 = val_ident COLON v4 = core_type EQUAL v6 = primitive_declaration v7 = post_item_attributes
    { mksig $startpos $endpos (Psig_value
                (Val.mk (mkrhs $startpos(v2) $endpos(v2) v2) v4 ~prim:v6 ~attrs:v7
                   ~loc:(rloc $startpos $endpos))) }
| TYPE v2 = type_declarations
    { mksig $startpos $endpos (Psig_type (List.rev v2)) }
| EXCEPTION v2 = exception_declaration
    { mksig $startpos $endpos (Psig_exception v2) }
| MODULE v2 = UIDENT v3 = module_declaration v4 = post_item_attributes
    { mksig $startpos $endpos (Psig_module (Md.mk (mkrhs $startpos(v2) $endpos(v2) v2)
                             v3 ~attrs:v4 ~loc:(rloc $startpos $endpos))) }
| MODULE v2 = UIDENT EQUAL v4 = mod_longident v5 = post_item_attributes
    { mksig $startpos $endpos (Psig_module (Md.mk (mkrhs $startpos(v2) $endpos(v2) v2)
                             (Mty.alias ~loc:(rloc $startpos(v4) $endpos(v)) (mkrhs $startpos(v4) $endpos(v4) v4))
                             ~attrs:v5
                             ~loc:(rloc $startpos $endpos)
                          )) }
| MODULE REC v3 = module_rec_declarations
    { mksig $startpos $endpos (Psig_recmodule (List.rev v3)) }
| MODULE TYPE v3 = ident v4 = post_item_attributes
    { mksig $startpos $endpos (Psig_modtype (Mtd.mk (mkrhs $startpos(v3) $endpos(v3) v3)
                              ~attrs:v4 ~loc:(rloc $startpos $endpos))) }
| MODULE TYPE v3 = ident EQUAL v5 = module_type v6 = post_item_attributes
    { mksig $startpos $endpos (Psig_modtype (Mtd.mk (mkrhs $startpos(v3) $endpos(v3) v3) ~typ:v5
                              ~loc:(rloc $startpos $endpos)
                              ~attrs:v6)) }
| OPEN v2 = override_flag v3 = mod_longident v4 = post_item_attributes
    { mksig $startpos $endpos (Psig_open (v2, mkrhs $startpos(v3) $endpos(v3) v3, v4)) }
| INCLUDE v2 = module_type v3 = post_item_attributes %prec below_WITH
    { mksig $startpos $endpos (Psig_include (v2, v3)) }
| CLASS v2 = class_descriptions
    { mksig $startpos $endpos (Psig_class (List.rev v2)) }
| CLASS TYPE v3 = class_type_declarations
    { mksig $startpos $endpos (Psig_class_type (List.rev v3)) }
| v1 = item_extension v2 = post_item_attributes
    { mksig $startpos $endpos (Psig_extension (v1, v2)) }

module_declaration:
| COLON v2 = module_type
    { v2 }
| LPAREN v2 = UIDENT COLON v4 = module_type RPAREN v6 = module_declaration
    { mkmty $startpos $endpos (Pmty_functor(mkrhs $startpos(v2) $endpos(v2) v2, Some v4, v6)) }
| LPAREN RPAREN v3 = module_declaration
    { mkmty $startpos $endpos (Pmty_functor(mkrhs $startpos(v1) $endpos(v1) "()", None, v3)) }

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
    { v3 :: v1 }
| v1 = class_declaration
    { [v1] }

class_declaration:
| v1 = virtual_flag v2 = class_type_parameters v3 = LIDENT v4 = class_fun_binding v5 = post_item_attributes
    {
       Ci.mk (mkrhs $startpos(v3) $endpos(v3) v3) v4
         ~virt:v1 ~params:v2
         ~attrs:v5 ~loc:(rloc $startpos $endpos)
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
| LET v2 = rec_flag v3 = let_bindings IN v5 = class_expr
    { mkclass $startpos $endpos (Pcl_let (v2, List.rev v3, v5)) }
| v1 = class_expr v2 = attribute
    { Cl.attr v1 v2 }
| v1 = extension
    { mkclass $startpos $endpos (Pcl_extension v1) }

class_simple_expr:
| LBRACKET v2 = core_type_comma_list RBRACKET v4 = class_longident
    { mkclass $startpos $endpos (Pcl_constr(mkloc v4 (rloc $startpos(v4) $endpos(v)), List.rev v2)) }
| v1 = class_longident
    { mkclass $startpos $endpos (Pcl_constr(mkrhs $startpos(v1) $endpos(v1) v1, [])) }
| OBJECT v2 = class_structure END
    { mkclass $startpos $endpos (Pcl_structure(v2)) }
| OBJECT class_structure error
    { unclosed "object" 1 "end" 3 }
| LPAREN v2 = class_expr COLON v4 = class_type RPAREN
    { mkclass $startpos $endpos (Pcl_constraint(v2, v4)) }
| LPAREN class_expr COLON class_type error
    { unclosed "(" 1 ")" 5 }
| LPAREN v2 = class_expr RPAREN
    { v2 }
| LPAREN class_expr error
    { unclosed "(" 1 ")" 3 }

class_structure:
| v1 = class_self_pattern v2 = class_fields
    { Cstr.mk v1 (List.rev v2) }

class_self_pattern:
| LPAREN v2 = pattern RPAREN
    { reloc_pat v2 }
| LPAREN v2 = pattern COLON v4 = core_type RPAREN
    { mkpat $startpos $endpos (Ppat_constraint(v2, v4)) }
| (* empty *)
    { ghpat(Ppat_any) }

class_fields:
| (* empty *)
    { [] }
| v1 = class_fields v2 = class_field
    { v2 :: v1 }

class_field:
| INHERIT v2 = override_flag v3 = class_expr v4 = parent_binder
    { mkcf $startpos $endpos  (Pcf_inherit (v2, v3, v4)) }
| VAL v2 = value
    { mkcf $startpos $endpos  (Pcf_val v2) }
| METHOD v2 = method_
    { mkcf $startpos $endpos  (Pcf_method v2) }
| CONSTRAINT v2 = constrain_field
    { mkcf $startpos $endpos  (Pcf_constraint v2) }
| INITIALIZER v2 = seq_expr
    { mkcf $startpos $endpos  (Pcf_initializer v2) }
| v1 = class_field v2 = post_item_attribute
    { Cf.attr v1 v2 }
| v1 = item_extension
    { mkcf $startpos $endpos (Pcf_extension v1) }

parent_binder:
| AS v2 = LIDENT
    { Some v2 }
| (* empty *)
    { None }

value:
(* TODO: factorize these rules (also with method): *)
| v1 = override_flag MUTABLE VIRTUAL v4 = label COLON v6 = core_type
    { if v1 = Override then syntax_error ();
        mkloc v4 (rloc $startpos(v4) $endpos(v)), Mutable, Cfk_virtual v6 }
| VIRTUAL v2 = mutable_flag v3 = label COLON v5 = core_type
    { mkrhs $startpos(v3) $endpos(v3) v3, v2, Cfk_virtual v5 }
| v1 = override_flag v2 = mutable_flag v3 = label EQUAL v5 = seq_expr
    { mkrhs $startpos(v3) $endpos(v3) v3, v2, Cfk_concrete (v1, v5) }
| v1 = override_flag v2 = mutable_flag v3 = label v4 = type_constraint EQUAL v6 = seq_expr
    {
       let e = mkexp_constraint v6 v4 in
       mkrhs $startpos(v3) $endpos(v3) v3, v2, Cfk_concrete (v1, e)
      }

method_:
(* TODO: factorize those rules... *)
| v1 = override_flag PRIVATE VIRTUAL v4 = label COLON v6 = poly_type
    { if v1 = Override then syntax_error ();
        mkloc v4 (rloc $startpos(v4) $endpos(v)), Private, Cfk_virtual v6 }
| v1 = override_flag VIRTUAL v3 = private_flag v4 = label COLON v6 = poly_type
    { if v1 = Override then syntax_error ();
        mkloc v4 (rloc $startpos(v4) $endpos(v)), v3, Cfk_virtual v6 }
| v1 = override_flag v2 = private_flag v3 = label v4 = strict_binding
    { mkloc v3 (rloc $startpos(v3) $endpos(v)), v2, Cfk_concrete (v1, ghexp(Pexp_poly (v4, None))) }
| v1 = override_flag v2 = private_flag v3 = label COLON v5 = poly_type EQUAL v7 = seq_expr
    { mkloc v3 (rloc $startpos(v3) $endpos(v)), v2, Cfk_concrete (v1, ghexp(Pexp_poly(v7, Some v5))) }
| v1 = override_flag v2 = private_flag v3 = label COLON TYPE v6 = lident_list DOT v8 = core_type EQUAL $10 = seq_expr
    { let exp, poly = wrap_type_annotation v6 v8 $10 in
        mkloc v3 (rloc $startpos(v3) $endpos(v)), v2, Cfk_concrete (v1, ghexp(Pexp_poly(exp, Some poly))) }

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
    { mkcty $startpos $endpos (Pcty_constr (mkloc v4 (rloc $startpos(v4) $endpos(v)), List.rev v2)) }
| v1 = clty_longident
    { mkcty $startpos $endpos (Pcty_constr (mkrhs $startpos(v1) $endpos(v1) v1, [])) }
| OBJECT v2 = class_sig_body END
    { mkcty $startpos $endpos (Pcty_signature v2) }
| OBJECT class_sig_body error
    { unclosed "object" 1 "end" 3 }

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
| INHERIT v2 = class_signature
    { mkctf $startpos $endpos  (Pctf_inherit v2) }
| VAL v2 = value_type
    { mkctf $startpos $endpos  (Pctf_val v2) }
| METHOD v2 = private_virtual_flags v3 = label COLON v5 = poly_type
    {
       let (p, v) = v2 in
       mkctf $startpos $endpos  (Pctf_method (v3, p, v, v5))
      }
| CONSTRAINT v2 = constrain_field
    { mkctf $startpos $endpos  (Pctf_constraint v2) }
| v1 = class_sig_field v2 = post_item_attribute
    { Ctf.attr v1 v2 }
| v1 = item_extension
    { mkctf $startpos $endpos (Pctf_extension v1) }

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
    { v3 :: v1 }
| v1 = class_description
    { [v1] }

class_description:
| v1 = virtual_flag v2 = class_type_parameters v3 = LIDENT COLON v5 = class_type v6 = post_item_attributes
    {
       Ci.mk (mkrhs $startpos(v3) $endpos(v3) v3) v5
         ~virt:v1 ~params:v2
         ~attrs:v6 ~loc:(rloc $startpos $endpos)
      }

class_type_declarations:
| v1 = class_type_declarations AND v3 = class_type_declaration
    { v3 :: v1 }
| v1 = class_type_declaration
    { [v1] }

class_type_declaration:
| v1 = virtual_flag v2 = class_type_parameters v3 = LIDENT EQUAL v5 = class_signature v6 = post_item_attributes
    {
       Ci.mk (mkrhs $startpos(v3) $endpos(v3) v3) v5
         ~virt:v1 ~params:v2
         ~attrs:v6 ~loc:(rloc $startpos $endpos)
      }

(* Core expressions *)

seq_expr:
| v1 = expr %prec below_SEMI
    { v1 }
| v1 = expr SEMI
    { reloc_exp v1 }
| v1 = expr SEMI v3 = seq_expr
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
| LET v2 = ext_attributes v3 = rec_flag v4 = let_bindings IN v6 = seq_expr
    { mkexp_attrs (Pexp_let(v3, List.rev v4, v6)) v2 }
| LET MODULE v3 = ext_attributes v4 = UIDENT v5 = module_binding_body IN v7 = seq_expr
    { mkexp_attrs (Pexp_letmodule(mkrhs $startpos(v4) $endpos(v4) v4, v5, v7)) v3 }
| LET OPEN v3 = override_flag v4 = ext_attributes v5 = mod_longident IN v7 = seq_expr
    { mkexp_attrs (Pexp_open(v3, mkrhs $startpos(v5) $endpos(v5) v5, v7)) v4 }
| FUNCTION v2 = ext_attributes opt_bar v4 = match_cases
    { mkexp_attrs (Pexp_function(List.rev v4)) v2 }
| FUN v2 = ext_attributes v3 = labeled_simple_pattern v4 = fun_def
    { let (l,o,p) = v3 in
        mkexp_attrs (Pexp_fun(l, o, p, v4)) v2 }
| FUN v2 = ext_attributes LPAREN TYPE v5 = LIDENT RPAREN v7 = fun_def
    { mkexp_attrs (Pexp_newtype(v5, v7)) v2 }
| MATCH v2 = ext_attributes v3 = seq_expr WITH opt_bar v6 = match_cases
    { mkexp_attrs (Pexp_match(v3, List.rev v6)) v2 }
| TRY v2 = ext_attributes v3 = seq_expr WITH opt_bar v6 = match_cases
    { mkexp_attrs (Pexp_try(v3, List.rev v6)) v2 }
| TRY ext_attributes seq_expr WITH error
    { syntax_error() }
| v1 = expr_comma_list %prec below_COMMA
    { mkexp $startpos $endpos (Pexp_tuple(List.rev v1)) }
| v1 = constr_longident v2 = simple_expr %prec below_SHARP
    { mkexp $startpos $endpos (Pexp_construct(mkrhs $startpos(v1) $endpos(v1) v1, Some v2)) }
| v1 = name_tag v2 = simple_expr %prec below_SHARP
    { mkexp $startpos $endpos (Pexp_variant(v1, Some v2)) }
| IF v2 = ext_attributes v3 = seq_expr THEN v5 = expr ELSE v7 = expr
    { mkexp_attrs(Pexp_ifthenelse(v3, v5, Some v7)) v2 }
| IF v2 = ext_attributes v3 = seq_expr THEN v5 = expr
    { mkexp_attrs (Pexp_ifthenelse(v3, v5, None)) v2 }
| WHILE v2 = ext_attributes v3 = seq_expr DO v5 = seq_expr DONE
    { mkexp_attrs (Pexp_while(v3, v5)) v2 }
| FOR v2 = ext_attributes v3 = pattern EQUAL v5 = seq_expr v6 = direction_flag v7 = seq_expr DO v9 = seq_expr DONE
    { mkexp_attrs(Pexp_for(v3, v5, v7, v6, v9)) v2 }
| v1 = expr COLONCOLON v3 = expr
    { mkexp_cons (rloc $startpos(v2) $endpos(v)) (ghexp(Pexp_tuple[v1;v3])) (rloc $startpos $endpos) }
| LPAREN COLONCOLON RPAREN LPAREN v5 = expr COMMA v7 = expr RPAREN
    { mkexp_cons (rloc $startpos(v2) $endpos(v)) (ghexp(Pexp_tuple[v5;v7])) (rloc $startpos $endpos) }
| v1 = expr v2 = INFIXOP0 v3 = expr
    { mkinfix v1 v2 v3 }
| v1 = expr v2 = INFIXOP1 v3 = expr
    { mkinfix v1 v2 v3 }
| v1 = expr v2 = INFIXOP2 v3 = expr
    { mkinfix v1 v2 v3 }
| v1 = expr v2 = INFIXOP3 v3 = expr
    { mkinfix v1 v2 v3 }
| v1 = expr v2 = INFIXOP4 v3 = expr
    { mkinfix v1 v2 v3 }
| v1 = expr PLUS v3 = expr
    { mkinfix v1 "+" v3 }
| v1 = expr PLUSDOT v3 = expr
    { mkinfix v1 "+." v3 }
| v1 = expr MINUS v3 = expr
    { mkinfix v1 "-" v3 }
| v1 = expr MINUSDOT v3 = expr
    { mkinfix v1 "-." v3 }
| v1 = expr STAR v3 = expr
    { mkinfix v1 "*" v3 }
| v1 = expr PERCENT v3 = expr
    { mkinfix v1 "%" v3 }
| v1 = expr EQUAL v3 = expr
    { mkinfix v1 "=" v3 }
| v1 = expr LESS v3 = expr
    { mkinfix v1 "<" v3 }
| v1 = expr GREATER v3 = expr
    { mkinfix v1 ">" v3 }
| v1 = expr OR v3 = expr
    { mkinfix v1 "or" v3 }
| v1 = expr BARBAR v3 = expr
    { mkinfix v1 "||" v3 }
| v1 = expr AMPERSAND v3 = expr
    { mkinfix v1 "&" v3 }
| v1 = expr AMPERAMPER v3 = expr
    { mkinfix v1 "&&" v3 }
| v1 = expr COLONEQUAL v3 = expr
    { mkinfix v1 ":=" v3 }
| v1 = subtractive v2 = expr %prec prec_unary_minus
    { mkuminus v1 v2 }
| v1 = additive v2 = expr %prec prec_unary_plus
    { mkuplus v1 v2 }
| v1 = simple_expr DOT v3 = label_longident LESSMINUS v5 = expr
    { mkexp $startpos $endpos (Pexp_setfield(v1, mkrhs $startpos(v3) $endpos(v3) v3, v5)) }
| v1 = simple_expr DOT LPAREN v4 = seq_expr RPAREN LESSMINUS v7 = expr
    { mkexp $startpos $endpos (Pexp_apply(ghexp(Pexp_ident(array_function "Array" "set")),
                         ["",v1; "",v4; "",v7])) }
| v1 = simple_expr DOT LBRACKET v4 = seq_expr RBRACKET LESSMINUS v7 = expr
    { mkexp $startpos $endpos (Pexp_apply(ghexp(Pexp_ident(array_function "String" "set")),
                         ["",v1; "",v4; "",v7])) }
| v1 = simple_expr DOT LBRACE v4 = expr RBRACE LESSMINUS v7 = expr
    { bigarray_set v1 v4 v7 }
| v1 = label LESSMINUS v3 = expr
    { mkexp $startpos $endpos (Pexp_setinstvar(mkrhs $startpos(v1) $endpos(v1) v1, v3)) }
| ASSERT v2 = ext_attributes v3 = simple_expr %prec below_SHARP
    { mkexp_attrs (Pexp_assert v3) v2 }
| LAZY v2 = ext_attributes v3 = simple_expr %prec below_SHARP
    { mkexp_attrs (Pexp_lazy v3) v2 }
| OBJECT v2 = ext_attributes v3 = class_structure END
    { mkexp_attrs (Pexp_object v3) v2 }
| OBJECT ext_attributes class_structure error
    { unclosed "object" 1 "end" 3 }
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
| LPAREN v2 = seq_expr RPAREN
    { reloc_exp v2 }
| LPAREN seq_expr error
    { unclosed "(" 1 ")" 3 }
| BEGIN v2 = ext_attributes v3 = seq_expr END
    { wrap_exp_attrs (reloc_exp v3) v2 (* check location *) }
| BEGIN v2 = ext_attributes END
    { mkexp_attrs (Pexp_construct (mkloc (Lident "()") (rloc $startpos $endpos),
                               None)) v2 }
| BEGIN ext_attributes seq_expr error
    { unclosed "begin" 1 "end" 3 }
| LPAREN v2 = seq_expr v3 = type_constraint RPAREN
    { mkexp_constraint v2 v3 }
| v1 = simple_expr DOT v3 = label_longident
    { mkexp $startpos $endpos (Pexp_field(v1, mkrhs $startpos(v3) $endpos(v3) v3)) }
| v1 = mod_longident DOT LPAREN v4 = seq_expr RPAREN
    { mkexp $startpos $endpos (Pexp_open(Fresh, mkrhs $startpos(v1) $endpos(v1) v1, v4)) }
| mod_longident DOT LPAREN seq_expr error
    { unclosed "(" 3 ")" 5 }
| v1 = simple_expr DOT LPAREN v4 = seq_expr RPAREN
    { mkexp $startpos $endpos (Pexp_apply(ghexp(Pexp_ident(array_function "Array" "get")),
                         ["",v1; "",v4])) }
| simple_expr DOT LPAREN seq_expr error
    { unclosed "(" 3 ")" 5 }
| v1 = simple_expr DOT LBRACKET v4 = seq_expr RBRACKET
    { mkexp $startpos $endpos (Pexp_apply(ghexp(Pexp_ident(array_function "String" "get")),
                         ["",v1; "",v4])) }
| simple_expr DOT LBRACKET seq_expr error
    { unclosed "[" 3 "]" 5 }
| v1 = simple_expr DOT LBRACE v4 = expr RBRACE
    { bigarray_get v1 v4 }
| simple_expr DOT LBRACE expr_comma_list error
    { unclosed "{" 3 "}" 5 }
| LBRACE v2 = record_expr RBRACE
    { let (exten, fields) = v2 in mkexp $startpos $endpos  (Pexp_record(fields, exten)) }
| LBRACE record_expr error
    { unclosed "{" 1 "}" 3 }
| v1 = mod_longident DOT LBRACE v4 = record_expr RBRACE
    { let (exten, fields) = v4 in
        let rec_exp = mkexp $startpos $endpos (Pexp_record(fields, exten)) in
        mkexp $startpos $endpos (Pexp_open(Fresh, mkrhs $startpos(v1) $endpos(v1) v1, rec_exp)) }
| mod_longident DOT LBRACE record_expr error
    { unclosed "{" 3 "}" 5 }
| LBRACKETBAR v2 = expr_semi_list opt_semi BARRBRACKET
    { mkexp $startpos $endpos  (Pexp_array(List.rev v2)) }
| LBRACKETBAR expr_semi_list opt_semi error
    { unclosed "[|" 1 "|]" 4 }
| LBRACKETBAR BARRBRACKET
    { mkexp $startpos $endpos  (Pexp_array []) }
| v1 = mod_longident DOT LBRACKETBAR v4 = expr_semi_list opt_semi BARRBRACKET
    { mkexp $startpos $endpos (Pexp_open(Fresh, mkrhs $startpos(v1) $endpos(v1) v1, mkexp(Pexp_array(List.rev v4)))) }
| mod_longident DOT LBRACKETBAR expr_semi_list opt_semi error
    { unclosed "[|" 3 "|]" 6 }
| LBRACKET v2 = expr_semi_list opt_semi RBRACKET
    { reloc_exp (mktailexp (rloc $startpos(v4) $endpos(v)) (List.rev v2)) }
| LBRACKET expr_semi_list opt_semi error
    { unclosed "[" 1 "]" 4 }
| v1 = mod_longident DOT LBRACKET v4 = expr_semi_list opt_semi RBRACKET
    { let list_exp = reloc_exp (mktailexp (rloc $startpos(v6) $endpos(v)) (List.rev v4)) in
        mkexp $startpos $endpos (Pexp_open(Fresh, mkrhs $startpos(v1) $endpos(v1) v1, list_exp)) }
| mod_longident DOT LBRACKET expr_semi_list opt_semi error
    { unclosed "[" 3 "]" 6 }
| v1 = PREFIXOP v2 = simple_expr
    { mkexp $startpos $endpos (Pexp_apply(mkoperator v1 1, ["",v2])) }
| BANG v2 = simple_expr
    { mkexp $startpos $endpos (Pexp_apply(mkoperator "!" 1, ["",v2])) }
| NEW v2 = ext_attributes v3 = class_longident
    { mkexp_attrs (Pexp_new(mkrhs $startpos(v3) $endpos(v3) v3)) v2 }
| LBRACELESS v2 = field_expr_list opt_semi GREATERRBRACE
    { mkexp $startpos $endpos  (Pexp_override(List.rev v2)) }
| LBRACELESS field_expr_list opt_semi error
    { unclosed "{<" 1 ">}" 4 }
| LBRACELESS GREATERRBRACE
    { mkexp $startpos $endpos  (Pexp_override [])}
| v1 = mod_longident DOT LBRACELESS v4 = field_expr_list opt_semi GREATERRBRACE
    { mkexp $startpos $endpos (Pexp_open(Fresh, mkrhs $startpos(v1) $endpos(v1) v1, mkexp (Pexp_override(List.rev v4)))) }
| mod_longident DOT LBRACELESS field_expr_list opt_semi error
    { unclosed "{<" 3 ">}" 6 }
| v1 = simple_expr SHARP v3 = label
    { mkexp $startpos $endpos (Pexp_send(v1, v3)) }
| LPAREN MODULE v3 = module_expr RPAREN
    { mkexp $startpos $endpos  (Pexp_pack v3) }
| LPAREN MODULE v3 = module_expr COLON v5 = package_type RPAREN
    { mkexp $startpos $endpos  (Pexp_constraint (ghexp (Pexp_pack v3),
                                ghtyp (Ptyp_package v5))) }
| LPAREN MODULE module_expr COLON error
    { unclosed "(" 1 ")" 5 }
| v1 = mod_longident DOT LPAREN MODULE v5 = module_expr COLON v7 = package_type RPAREN
    { mkexp $startpos $endpos (Pexp_open(Fresh, mkrhs $startpos(v1) $endpos(v1) v1,
        mkexp $startpos $endpos  (Pexp_constraint (ghexp (Pexp_pack v5),
                                ghtyp (Ptyp_package v7))))) }
| mod_longident DOT LPAREN MODULE module_expr COLON error
    { unclosed "(" 3 ")" 7 }
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
    { (v1, mkexp $startpos $endpos (Pexp_ident(mkrhs $startpos(v1) $endpos(v1) (Lidentv1)))) }

let_bindings:
| v1 = let_binding
    { [v1] }
| v1 = let_bindings AND v3 = let_binding
    { v3 :: v1 }

lident_list:
| v1 = LIDENT
    { [v1] }
| v1 = LIDENT v2 = lident_list
    { v1 :: v2 }

let_binding:
| v1 = let_binding_ v2 = post_item_attributes
    { let (p, e) = v1 in Vb.mk ~attrs:v2 p e }

let_binding_:
| v1 = val_ident v2 = fun_binding
    { (mkpat $startpos(v1) $endpos(v1) v1, v2) }
| v1 = val_ident COLON v3 = typevar_list DOT v5 = core_type EQUAL v7 = seq_expr
    { (ghpat(Ppat_constraint(mkpat $startpos(v1) $endpos(v1) v1,
                               ghtyp(Ptyp_poly(List.rev v3,v5)))),
         v7) }
| v1 = val_ident COLON TYPE v4 = lident_list DOT v6 = core_type EQUAL v8 = seq_expr
    { let exp, poly = wrap_type_annotation v4 v6 v8 in
        (ghpat(Ppat_constraint(mkpat $startpos(v1) $endpos(v1) v1, poly)), exp) }
| v1 = pattern EQUAL v3 = seq_expr
    { (v1, v3) }
| v1 = simple_pattern_not_ident COLON v3 = core_type EQUAL v5 = seq_expr
    { (ghpat(Ppat_constraint(v1, v3)), v5) }

fun_binding:
| v1 = strict_binding
    { v1 }
| v1 = type_constraint EQUAL v3 = seq_expr
    { mkexp_constraint v3 v1 }

strict_binding:
| EQUAL v2 = seq_expr
    { v2 }
| v1 = labeled_simple_pattern v2 = fun_binding
    { let (l, o, p) = v1 in ghexp(Pexp_fun(l, o, p, v2)) }
| LPAREN TYPE v3 = LIDENT RPAREN v5 = fun_binding
    { mkexp $startpos $endpos (Pexp_newtype(v3, v5)) }

match_cases:
| v1 = match_case
    { [v1] }
| v1 = match_cases BAR v3 = match_case
    { v3 :: v1 }

match_case:
| v1 = pattern MINUSGREATER v3 = seq_expr
    { Exp.case v1 v3 }
| v1 = pattern WHEN v3 = seq_expr MINUSGREATER v5 = seq_expr
    { Exp.case v1 ~guard:v3 v5 }

fun_def:
| MINUSGREATER v2 = seq_expr
(* Cf #5939: we used to accept (fun p when e0 -> e) *)
    { v2 }
| v1 = labeled_simple_pattern v2 = fun_def
    {
       let (l,o,p) = v1 in
       ghexp(Pexp_fun(l, o, p, v2))
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
    { (mkrhs $startpos(v1) $endpos(v1) v1, exp_of_label v1 1) }

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
| COLON v2 = core_type
    { (Some v2, None) }
| COLON v2 = core_type COLONGREATER v4 = core_type
    { (Some v2, Some v4) }
| COLONGREATER v2 = core_type
    { (None, Some v2) }
| COLON error
    { syntax_error() }
| COLONGREATER error
    { syntax_error() }

(* Patterns *)

pattern:
| v1 = simple_pattern
    { v1 }
| v1 = pattern AS v3 = val_ident
    { mkpat $startpos $endpos (Ppat_alias(v1, mkrhs $startpos(v3) $endpos(v3) v3)) }
| pattern AS error
    { expecting 3 "identifier" }
| v1 = pattern_comma_list %prec below_COMMA
    { mkpat $startpos $endpos (Ppat_tuple(List.rev v1)) }
| v1 = constr_longident v2 = pattern %prec prec_constr_appl
    { mkpat $startpos $endpos (Ppat_construct(mkrhs $startpos(v1) $endpos(v1) v1, Some v2)) }
| v1 = name_tag v2 = pattern %prec prec_constr_appl
    { mkpat $startpos $endpos (Ppat_variant(v1, Some v2)) }
| v1 = pattern COLONCOLON v3 = pattern
    { mkpat_cons (rloc $startpos(v2) $endpos(v)) (ghpat(Ppat_tuple[v1;v3])) (rloc $startpos $endpos) }
| pattern COLONCOLON error
    { expecting 3 "pattern" }
| LPAREN COLONCOLON RPAREN LPAREN v5 = pattern COMMA v7 = pattern RPAREN
    { mkpat_cons (rloc $startpos(v2) $endpos(v)) (ghpat(Ppat_tuple[v5;v7])) (rloc $startpos $endpos) }
| LPAREN COLONCOLON RPAREN LPAREN pattern COMMA pattern error
    { unclosed "(" 4 ")" 8 }
| v1 = pattern BAR v3 = pattern
    { mkpat $startpos $endpos (Ppat_or(v1, v3)) }
| pattern BAR error
    { expecting 3 "pattern" }
| LAZY v2 = simple_pattern
    { mkpat $startpos $endpos (Ppat_lazy v2) }
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
| LBRACE v2 = lbl_pattern_list RBRACE
    { let (fields, closed) = v2 in mkpat $startpos $endpos (Ppat_record(fields, closed)) }
| LBRACE lbl_pattern_list error
    { unclosed "{" 1 "}" 3 }
| LBRACKET v2 = pattern_semi_list opt_semi RBRACKET
    { reloc_pat (mktailpat (rloc $startpos(v4) $endpos(v)) (List.rev v2)) }
| LBRACKET pattern_semi_list opt_semi error
    { unclosed "[" 1 "]" 4 }
| LBRACKETBAR v2 = pattern_semi_list opt_semi BARRBRACKET
    { mkpat $startpos $endpos (Ppat_array(List.rev v2)) }
| LBRACKETBAR BARRBRACKET
    { mkpat $startpos $endpos (Ppat_array []) }
| LBRACKETBAR pattern_semi_list opt_semi error
    { unclosed "[|" 1 "|]" 4 }
| LPAREN v2 = pattern RPAREN
    { reloc_pat v2 }
| LPAREN pattern error
    { unclosed "(" 1 ")" 3 }
| LPAREN v2 = pattern COLON v4 = core_type RPAREN
    { mkpat $startpos $endpos (Ppat_constraint(v2, v4)) }
| LPAREN pattern COLON core_type error
    { unclosed "(" 1 ")" 5 }
| LPAREN pattern COLON error
    { expecting 4 "type" }
| LPAREN MODULE v3 = UIDENT RPAREN
    { mkpat $startpos $endpos (Ppat_unpack (mkrhs $startpos(v3) $endpos(v3) v3)) }
| LPAREN MODULE v3 = UIDENT COLON v5 = package_type RPAREN
    { mkpat $startpos $endpos (Ppat_constraint(mkpat(Ppat_unpack (mkrhs $startpos(v3) $endpos(v3) v3)),
                              ghtyp(Ptyp_package v5))) }
| LPAREN MODULE UIDENT COLON package_type error
    { unclosed "(" 1 ")" 6 }
| v1 = extension
    { mkpat $startpos $endpos (Ppat_extension v1) }

pattern_comma_list:
| v1 = pattern_comma_list COMMA v3 = pattern
    { v3 :: v1 }
| v1 = pattern COMMA v3 = pattern
    { [v3; v1] }
| pattern COMMA error
    { expecting 3 "pattern" }

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
    { (mkrhs $startpos(v1) $endpos(v1) v1, pat_of_label v1 1) }

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

optional_type_parameters:
| (* empty *)
    { [] }
| v1 = optional_type_parameter
    { [v1] }
| LPAREN v2 = optional_type_parameter_list RPAREN
    { List.rev v2 }

optional_type_parameter:
| v1 = type_variance QUOTE v3 = ident
    { Some (mkrhs $startpos(v3) $endpos(v3) v3), v1 }
| v1 = type_variance UNDERSCORE
    { None, v1 }

optional_type_parameter_list:
| v1 = optional_type_parameter
    { [v1] }
| v1 = optional_type_parameter_list COMMA v3 = optional_type_parameter
    { v3 :: v1 }

type_parameters:
| (* empty *)
    { [] }
| v1 = type_parameter
    { [v1] }
| LPAREN v2 = type_parameter_list RPAREN
    { List.rev v2 }

type_parameter:
| v1 = type_variance QUOTE v3 = ident
    { mkrhs $startpos(v3) $endpos(v3) v3, v1 }

type_variance:
| (* empty *)
    { Invariant }
| PLUS
    { Covariant }
| MINUS
    { Contravariant }

type_parameter_list:
| v1 = type_parameter
    { [v1] }
| v1 = type_parameter_list COMMA v3 = type_parameter
    { v3 :: v1 }

constructor_declarations:
| v1 = constructor_declaration
    { [v1] }
| v1 = constructor_declarations BAR v3 = constructor_declaration
    { v3 :: v1 }

constructor_declaration:
| v1 = constr_ident v2 = attributes v3 = generalized_constructor_arguments
    {
       let args,res = v3 in
       Type.constructor (mkrhs $startpos(v1) $endpos(v1) v1) ~args ?res ~loc:(rloc $startpos $endpos) ~attrs:v2
      }

exception_declaration:
| v1 = constructor_declaration v2 = post_item_attributes
    {
        let cd = v1 in
        {cd with pcd_attributes = cd.pcd_attributes @ v2}
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


(* "with" constraints (additional type equations over signature components) *)

with_constraints:
| v1 = with_constraint
    { [v1] }
| v1 = with_constraints AND v3 = with_constraint
    { v3 :: v1 }

with_constraint:
| TYPE v2 = type_parameters v3 = label_longident v4 = with_type_binder v5 = core_type v6 = constraints
    { Pwith_type
          (mkrhs $startpos(v3) $endpos(v3) v3,
           (Type.mk (mkrhs $startpos(v3) $endpos(v3) (Longident.lastv3))
              ~params:(List.map (fun (x, v) -> Some x, v) v2)
              ~cstrs:(List.rev v6)
              ~manifest:v5
              ~priv:v4
              ~loc:(rloc $startpos $endpos))) }
| TYPE v2 = type_parameters v3 = label COLONEQUAL v5 = core_type
    { Pwith_typesubst
          (Type.mk (mkrhs $startpos(v3) $endpos(v3) v3)
             ~params:(List.map (fun (x, v) -> Some x, v) v2)
             ~manifest:v5
             ~loc:(rloc $startpos $endpos)) }
| MODULE v2 = mod_longident EQUAL v4 = mod_ext_longident
    { Pwith_module (mkrhs $startpos(v2) $endpos(v2) v2, mkrhs $startpos(v4) $endpos(v4) v4) }
| MODULE v2 = UIDENT COLONEQUAL v4 = mod_ext_longident
    { Pwith_modsubst (mkrhs $startpos(v2) $endpos(v2) v2, mkrhs $startpos(v4) $endpos(v4) v4) }

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
    { match v2 with [sty] -> sty | _ -> raise Parse_error }
| v1 = simple_core_type v2 = attribute
    { Typ.attr v1 v2 }

simple_core_type_no_attr:
| v1 = simple_core_type2 %prec below_SHARP
    { v1 }
| LPAREN v2 = core_type_comma_list RPAREN %prec below_SHARP
    { match v2 with [sty] -> sty | _ -> raise Parse_error }

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
| v1 = name_tag OF v3 = opt_ampersand v4 = amper_type_list
    { Rtag (v1, v3, List.rev v4) }
| v1 = name_tag
    { Rtag (v1, true, []) }

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
| v1 = label COLON v3 = poly_type
    { (v1, v3) }

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
| LPAREN v2 = operator RPAREN
    { v2 }
| LPAREN operator error
    { unclosed "(" 1 ")" 3 }
| LPAREN error
    { expecting 2 "operator" }
| LPAREN MODULE error
    { expecting 3 "module-expr" }

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

(* Toplevel directives *)

toplevel_directive:
| SHARP v2 = ident
    { Ptop_dir(v2, Pdir_none) }
| SHARP v2 = ident v3 = STRING
    { Ptop_dir(v2, Pdir_string (fst v3)) }
| SHARP v2 = ident v3 = INT
    { Ptop_dir(v2, Pdir_int v3) }
| SHARP v2 = ident v3 = val_longident
    { Ptop_dir(v2, Pdir_ident v3) }
| SHARP v2 = ident FALSE
    { Ptop_dir(v2, Pdir_bool false) }
| SHARP v2 = ident TRUE
    { Ptop_dir(v2, Pdir_bool true) }

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
| LBRACKETPERCENTPERCENT v2 = attr_id v3 = payload RBRACKET
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

%%





