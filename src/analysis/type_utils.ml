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
    | `Reject -> None
    | `Accept _ -> assert false
  in
  parse (`Step (Raw_parser.initial Raw_parser.parse_expression_state
                  (Lexing.dummy_pos,Raw_parser.ENTRYPOINT,Lexing.dummy_pos)))

let type_in_env ?keywords env ppf expr =
  let print_expr expression =
    let (str, _sg, _) =
      Typemod.type_toplevel_phrase env
        Parsetree.([{ pstr_desc = Pstr_eval (expression, []) ; pstr_loc = Location.none }])
    in
    (*let sg' = Typemod.simplify_signature sg in*)
    let open Typedtree in
    begin match str.str_items with
      | [ { str_desc = Tstr_eval (exp,[]) }] ->
        Printtyp.type_scheme ppf exp.exp_type;
      | _ -> failwith "unhandled expression"
    end
  in
  Printtyp.wrap_printing_env env

    begin fun () ->
      match parse_expr ?keywords expr with (*
      | { Parsetree.pexp_desc = Parsetree.Pexp_construct (longident,None,_) } ->
        begin
          try let c = Typing_aux.lookup_constructor longident.Asttypes.txt env in
            Browse_misc.print_constructor ppf c
          with Not_found ->
            try let _, m = Env.lookup_module longident.Asttypes.txt env in
              Printtyp.modtype ppf m
            with Not_found ->
              try let p, m = Env.lookup_modtype longident.Asttypes.txt env in
                Printtyp.modtype_declaration (Ident.create (Path.last p)) ppf m
              with Not_found ->
                ()
        end *)
      | None ->
        Format.pp_print_string ppf "Syntax error";
        false

      | Some ({ Parsetree.pexp_desc = Parsetree.Pexp_ident longident } as e) ->
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

      | Some ({ Parsetree.pexp_desc = Parsetree.Pexp_construct (longident, _) } as e) ->
        begin
          try
            print_expr e;
            true
          with exn ->
            try
              let p = Env.lookup_module ~load:true longident.Asttypes.txt env in
              let md = Env.find_module p env in
              Printtyp.modtype ppf md.Types.md_type;
              true
            with _ ->
              raise exn
        end

      | Some e -> print_expr e; true
    end
