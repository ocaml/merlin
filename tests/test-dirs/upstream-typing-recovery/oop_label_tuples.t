  $ cat >oop_label_tuples.ml <<'EOF'
  > module type T = sig
  >   val foo : int -> int
  > end
  > 
  > module H = struct
  >   let foo = succ
  > end
  > 
  > module F = struct
  >   let f (module S : T) g x : int =
  >     let y = g (module S) (x + 1) in
  >     y + 220.2
  > 
  >   let on_obj handler x =
  >     (handler # from (handler # deal x)) ^ handler # finalize
  > 
  >   let g =
  >     let x = f (module H) (fun (module H : T) x -> H.foo x) |> on_obj object
  >       end
  >     in
  >     on_obj object
  >       method deal = String.to_seq
  >       method from = String.of_seq
  >       method finalize = 44
  >     end (x ^ "foo")
  > end
  > 
  > let fgh (~a, ~b, ~c) = (a + b + (int_of_string c))
  > 
  > let u = fgh (1, 2, 3) + fgh (~a:11, ~b:300, ~c:3000) + fgh (~a:22, ~b:1, ~c:"30")
  > 
  > type r =
  >   | Foo of { bar: int; baz: string option}
  > 
  > let f = function
  >   | Foo {bar; _} -> (int_of_string bar) + 10
  > 
  > let g x = 10
  > EOF

  $ $MERLIN single errors -filename oop_label_tuples.ml < oop_label_tuples.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 11,
          "col": 14
        },
        "end": {
          "line": 11,
          "col": 24
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The signature for this packaged module couldn't be inferred."
      },
      {
        "start": {
          "line": 12,
          "col": 8
        },
        "end": {
          "line": 12,
          "col": 13
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The constant 220.2 has type float but an expression was expected of type int"
      },
      {
        "start": {
          "line": 18,
          "col": 69
        },
        "end": {
          "line": 19,
          "col": 9
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type <  > but an expression was expected of type
    < deal : 'a -> 'b; finalize : string; from : 'b -> string; .. >
  The first object type has no method deal"
      },
      {
        "start": {
          "line": 21,
          "col": 11
        },
        "end": {
          "line": 25,
          "col": 7
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type
    < deal : String.t -> unit -> char Seq.node; finalize : int;
      from : char Seq.t -> String.t >
  but an expression was expected of type
    < deal : String.t -> unit -> char Seq.node; finalize : string;
      from : (unit -> char Seq.node) -> string; .. >
  The method finalize has type int, but the expected method type was string"
      },
      {
        "start": {
          "line": 30,
          "col": 12
        },
        "end": {
          "line": 30,
          "col": 21
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type 'a * 'b * 'c but an expression was expected of type
    a:int * b:int * c:string
  A label a was expected"
      },
      {
        "start": {
          "line": 30,
          "col": 47
        },
        "end": {
          "line": 30,
          "col": 51
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The constant 3000 has type int but an expression was expected of type string"
      },
      {
        "start": {
          "line": 36,
          "col": 35
        },
        "end": {
          "line": 36,
          "col": 38
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The value bar has type int but an expression was expected of type string"
      }
    ],
    "notifications": []
  }
