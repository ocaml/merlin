  $ cat >illegal_letrec.ml <<'EOF'
  > type 'a t = A of 'a
  > let rec A x = A (A ())
  > 
  > let f ~x:_ ~y:_ ~z:_ = ()
  > 
  > let c =
  >   (* Should works, OCaml up to 5.4 *)
  >   let rec (_ as f) = fun () -> f () in
  >   f ()
  > 
  > let rec x = f ~x
  > let rec x = f ~x ~z:0
  > 
  > let g ~omitted_g ~given = fun ~omitted_f:_ ~given:_ -> given ~omitted_f:()
  > let rec f : omitted_g:_ -> omitted_f:_ -> given:_ -> _ =
  >   g ~given:(f ~omitted_g:() ~given:0)
  > 
  > 
  > let (a, b) = c, c
  > let t : string = 10
  > EOF

  $ $MERLIN single errors -filename illegal_letrec.ml < illegal_letrec.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 8
        },
        "end": {
          "line": 2,
          "col": 11
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Only variables are allowed as left-hand side of let rec"
      },
      {
        "start": {
          "line": 8,
          "col": 10
        },
        "end": {
          "line": 8,
          "col": 18
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Only variables are allowed as left-hand side of let rec"
      },
      {
        "start": {
          "line": 11,
          "col": 12
        },
        "end": {
          "line": 11,
          "col": 16
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This kind of expression is not allowed as right-hand side of let rec"
      },
      {
        "start": {
          "line": 12,
          "col": 12
        },
        "end": {
          "line": 12,
          "col": 21
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This kind of expression is not allowed as right-hand side of let rec"
      },
      {
        "start": {
          "line": 16,
          "col": 2
        },
        "end": {
          "line": 16,
          "col": 37
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This kind of expression is not allowed as right-hand side of let rec"
      },
      {
        "start": {
          "line": 20,
          "col": 17
        },
        "end": {
          "line": 20,
          "col": 19
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The constant 10 has type int but an expression was expected of type string"
      }
    ],
    "notifications": []
  }
