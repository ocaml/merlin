
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WORD of (
# 4 "src/sherlodoc/type_parser.mly"
      (string)
# 11 "src/sherlodoc/type_parser.ml"
  )
    | WILDCARD
    | STAR
    | POLY of (
# 5 "src/sherlodoc/type_parser.mly"
      (string)
# 18 "src/sherlodoc/type_parser.ml"
  )
    | PARENS_OPEN
    | PARENS_CLOSE
    | EOF
    | COMMA
    | ARROW
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState19
  | MenhirState13
  | MenhirState11
  | MenhirState7
  | MenhirState6
  | MenhirState4
  | MenhirState0

let rec _menhir_goto_list1_typ_COMMA_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Type_parsed.t list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Type_parsed.t))), _, (xs : (Type_parsed.t list))) = _menhir_stack in
        let _2 = () in
        let _v : (Type_parsed.t list) = 
# 50 "src/sherlodoc/type_parser.mly"
                                               ( x::xs )
# 61 "src/sherlodoc/type_parser.ml"
         in
        _menhir_goto_list1_typ_COMMA_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PARENS_CLOSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (ts : (Type_parsed.t list))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Type_parsed.t list) = 
# 44 "src/sherlodoc/type_parser.mly"
                                                  ( ts )
# 79 "src/sherlodoc/type_parser.ml"
             in
            _menhir_goto_typs _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_WORD_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (string list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (
# 4 "src/sherlodoc/type_parser.mly"
      (string)
# 101 "src/sherlodoc/type_parser.ml"
        ))) = _menhir_stack in
        let _v : (string list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 106 "src/sherlodoc/type_parser.ml"
         in
        _menhir_goto_list_WORD_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (ws : (string list)) = _v in
        let ((_menhir_stack, _menhir_s, (ts : (Type_parsed.t list))), (w : (
# 4 "src/sherlodoc/type_parser.mly"
      (string)
# 116 "src/sherlodoc/type_parser.ml"
        ))) = _menhir_stack in
        let _v : (Type_parsed.t) = 
# 29 "src/sherlodoc/type_parser.mly"
    (
      List.fold_left ( fun acc w ->
	  Type_parsed.Tycon (w, [acc])) (Type_parsed.Tycon (w, ts)) ws
    )
# 124 "src/sherlodoc/type_parser.ml"
         in
        _menhir_goto_typ1 _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Type_parsed.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (a : (Type_parsed.t))), _, (b : (Type_parsed.t))) = _menhir_stack in
        let _2 = () in
        let _v : (Type_parsed.t) = 
# 18 "src/sherlodoc/type_parser.mly"
                       ( Type_parsed.Arrow (a, b) )
# 147 "src/sherlodoc/type_parser.ml"
         in
        _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState19 | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | PARENS_OPEN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | POLY _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
            | WILDCARD ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | WORD _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
            | ARROW | COMMA | PARENS_CLOSE | STAR ->
                _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19)
        | PARENS_CLOSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Type_parsed.t))) = _menhir_stack in
            let _v : (Type_parsed.t list) = 
# 49 "src/sherlodoc/type_parser.mly"
           ( [x] )
# 180 "src/sherlodoc/type_parser.ml"
             in
            _menhir_goto_list1_typ_COMMA_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (t : (Type_parsed.t))) = _menhir_stack in
            let _2 = () in
            let _v : (Type_parsed.t) = 
# 13 "src/sherlodoc/type_parser.mly"
              ( t )
# 202 "src/sherlodoc/type_parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (Type_parsed.t)) = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (string list) = 
# 211 "<standard.mly>"
    ( [] )
# 222 "src/sherlodoc/type_parser.ml"
     in
    _menhir_goto_list_WORD_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 4 "src/sherlodoc/type_parser.mly"
      (string)
# 229 "src/sherlodoc/type_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | WORD _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | ARROW | COMMA | EOF | PARENS_CLOSE | STAR ->
        _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7

and _menhir_goto_list1_typ1_STAR_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Type_parsed.t list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Type_parsed.t list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Type_parsed.t))) = _menhir_stack in
        let _2 = () in
        let _v : (Type_parsed.t list) = 
# 50 "src/sherlodoc/type_parser.mly"
                                               ( x::xs )
# 257 "src/sherlodoc/type_parser.ml"
         in
        _menhir_goto_list1_typ1_STAR_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 | MenhirState4 | MenhirState19 | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Type_parsed.t list)) = _v in
        let _v : (Type_parsed.t) = 
# 22 "src/sherlodoc/type_parser.mly"
                         ( Type_parsed.tuple xs )
# 267 "src/sherlodoc/type_parser.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | PARENS_OPEN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | POLY _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
            | WILDCARD ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | WORD _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
            | ARROW | COMMA | EOF | PARENS_CLOSE | STAR ->
                _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack) MenhirState11)
        | COMMA | EOF | PARENS_CLOSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (t : (Type_parsed.t))) = _menhir_stack in
            let _v : (Type_parsed.t) = 
# 17 "src/sherlodoc/type_parser.mly"
           ( t )
# 295 "src/sherlodoc/type_parser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_typs : _menhir_env -> 'ttv_tail -> _menhir_state -> (Type_parsed.t list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | WORD _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | WORD _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
        | ARROW | COMMA | EOF | PARENS_CLOSE | STAR ->
            _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6)
    | ARROW | COMMA | EOF | PARENS_CLOSE | STAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (ts : (Type_parsed.t list))) = _menhir_stack in
        let _v : (Type_parsed.t) = 
# 27 "src/sherlodoc/type_parser.mly"
            ( Type_parsed.tuple ts )
# 334 "src/sherlodoc/type_parser.ml"
         in
        _menhir_goto_typ1 _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_typ1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Type_parsed.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | STAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PARENS_OPEN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | POLY _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
        | WILDCARD ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | WORD _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
        | ARROW | COMMA | EOF | PARENS_CLOSE | STAR ->
            _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack) MenhirState13)
    | ARROW | COMMA | EOF | PARENS_CLOSE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (Type_parsed.t))) = _menhir_stack in
        let _v : (Type_parsed.t list) = 
# 49 "src/sherlodoc/type_parser.mly"
           ( [x] )
# 372 "src/sherlodoc/type_parser.ml"
         in
        _menhir_goto_list1_typ1_STAR_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_typ0 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Type_parsed.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (t : (Type_parsed.t)) = _v in
    let _v : (Type_parsed.t list) = 
# 43 "src/sherlodoc/type_parser.mly"
           ( [t] )
# 390 "src/sherlodoc/type_parser.ml"
     in
    _menhir_goto_typs _menhir_env _menhir_stack _menhir_s _v

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Type_parsed.t) = 
# 26 "src/sherlodoc/type_parser.mly"
    ( Type_parsed.Wildcard )
# 430 "src/sherlodoc/type_parser.ml"
     in
    _menhir_goto_typ1 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 4 "src/sherlodoc/type_parser.mly"
      (string)
# 437 "src/sherlodoc/type_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (w : (
# 4 "src/sherlodoc/type_parser.mly"
      (string)
# 445 "src/sherlodoc/type_parser.ml"
    )) = _v in
    let _v : (Type_parsed.t) = 
# 38 "src/sherlodoc/type_parser.mly"
           ( Type_parsed.Tycon (w, []) )
# 450 "src/sherlodoc/type_parser.ml"
     in
    _menhir_goto_typ0 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Type_parsed.t) = 
# 36 "src/sherlodoc/type_parser.mly"
             ( Type_parsed.Wildcard )
# 462 "src/sherlodoc/type_parser.ml"
     in
    _menhir_goto_typ0 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 5 "src/sherlodoc/type_parser.mly"
      (string)
# 469 "src/sherlodoc/type_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (w : (
# 5 "src/sherlodoc/type_parser.mly"
      (string)
# 477 "src/sherlodoc/type_parser.ml"
    )) = _v in
    let _v : (Type_parsed.t) = 
# 37 "src/sherlodoc/type_parser.mly"
           ( Type_parsed.Tyvar w )
# 482 "src/sherlodoc/type_parser.ml"
     in
    _menhir_goto_typ0 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | PARENS_OPEN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | POLY _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | WILDCARD ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | WORD _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | ARROW | COMMA | PARENS_CLOSE | STAR ->
        _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and main : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Type_parsed.t) =
  fun lexer lexbuf ->
    let _menhir_env = {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = Obj.magic ();
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | PARENS_OPEN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | POLY _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | WILDCARD ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | WORD _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | ARROW | EOF | STAR ->
        _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 549 "src/sherlodoc/type_parser.ml"
