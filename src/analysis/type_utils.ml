open Merlin_lib

let parse_expr ?(keywords=Raw_lexer.keywords []) expr =
  let lexbuf = Lexing.from_string expr in
  let state = Raw_lexer.make keywords in
  let rec lex parser = function
    | Raw_lexer.Error (e,l) ->
      assert false
    | Raw_lexer.Refill f ->
      lex parser (f ())
    | Raw_lexer.Return token ->
      parse (`Step (Raw_parser.feed parser
                      (Lexing.dummy_pos,token,Lexing.dummy_pos)))
  and parse = function
    | `Step s -> parse (Raw_parser.step s)
    | `Feed p -> lex p (Raw_lexer.token_without_comments state lexbuf)
    | `Accept (Raw_parser.N_ (Raw_parser.N_parse_expression, e)) ->
      Some (e : Parsetree.expression)
    | `Reject _ -> None
    | `Accept _ -> assert false
  in
  parse (`Step (Raw_parser.initial Raw_parser.parse_expression_state
                  (Lexing.dummy_pos,Raw_parser.ENTRYPOINT,Lexing.dummy_pos)))

let lookup_module_or_modtype name env =
  try
    let path, mty = Merlin_types_custom.lookup_module name env in
    path, Some mty
  with Not_found ->
    let path, mdtype = Env.lookup_modtype name env in
    path, mdtype.Types.mtd_type

let type_in_env ?keywords env ppf expr =
  let print_expr expression =
    let (str, _sg, _) =
      Typemod.type_toplevel_phrase env [Ast_helper.Str.eval expression]
    in
    (*let sg' = Typemod.simplify_signature sg in*)
    let open Typedtree in
    let exp = Merlin_types_custom.dest_tstr_eval str in
    Printtyp.type_scheme ppf exp.exp_type;
  in
  Printtyp.wrap_printing_env env

    begin fun () ->
      match parse_expr ?keywords expr with

      | None ->
        Format.pp_print_string ppf "Syntax error";
        false

      | Some e ->
        begin match Merlin_types_custom.Parsetree.extract_specific_parsing_info e with
        | `Ident longident ->
          begin
            try
              print_expr e;
              true
            with exn ->
              try let p, t = Env.lookup_type longident.Asttypes.txt env in
              Printtyp.type_declaration (Ident.create (Path.last p)) ppf t;
              true
              with _ ->
                raise exn
          end

        | `Constr longident ->
          begin
            try
              print_expr e;
              true
            with exn ->
              try
                (* TODO: special processing for module aliases? *)
                match lookup_module_or_modtype longident.Asttypes.txt env with
                | _path, None ->
                  Format.pp_print_string ppf "(* abstract module *)";
                  true
                | _path, Some md -> Printtyp.modtype ppf md;
                  true
              with _ ->
                raise exn
          end

        | `Other -> print_expr e; true
        end
    end
