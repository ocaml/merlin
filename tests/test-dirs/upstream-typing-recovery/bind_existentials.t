  $ cat >bind_existentials.ml <<'EOF'
  > type _ ty = Int : int ty
  > type dyn = Dyn : 'a ty * 'a -> dyn
  > 
  > type _ expr =
  >   | Int : int -> int expr
  >   | Add : (int -> int -> int) expr
  >   | App : ('a -> 'b) expr * 'a expr -> 'b expr
  > 
  > 
  > let a = function Dyn (type a b) (a, x : a ty * b) -> ignore (x : b)
  > 
  > 
  > let rec eval : type t. t expr -> t = function
  >     Int n -> n
  >   | Add -> (+)
  >   | App (type a) (f, x : _ * a expr) -> eval f (eval x : a)
  > 
  > let rec test : type a. a expr -> a = function
  >   | Int (type b) (n : a) -> n
  >   | Add -> (+)
  >   | App (type b) (f, x : (b -> a) expr * _) -> test f (test x : b)
  > 
  > type _ th =
  >   | Thunk : 'a * ('a -> 'b) -> 'b th
  > 
  > let f1 (type a) : a th -> a = function
  >   | Thunk (type b) (x, f : b * (b -> _)) -> f x
  > 
  > let f2 (type a) : a th -> a = function
  >   | Thunk (type b c) (x, f : b * (b -> c)) -> f x
  > 
  > let t : string = 10
  > EOF

  $ $MERLIN single errors -filename bind_existentials.ml < bind_existentials.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 10,
          "col": 40
        },
        "end": {
          "line": 10,
          "col": 48
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The local name b can only be given to an existential variable
  introduced by this GADT constructor. The type annotation tries to bind it to
  the name a that is already bound."
      },
      {
        "start": {
          "line": 10,
          "col": 61
        },
        "end": {
          "line": 10,
          "col": 62
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound value x"
      },
      {
        "start": {
          "line": 19,
          "col": 22
        },
        "end": {
          "line": 19,
          "col": 23
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The local name b can only be given to an existential variable
  introduced by this GADT constructor. The type annotation tries to bind it to
  the type 'a that is not a locally abstract type."
      },
      {
        "start": {
          "line": 19,
          "col": 28
        },
        "end": {
          "line": 19,
          "col": 29
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound value n"
      },
      {
        "start": {
          "line": 30,
          "col": 29
        },
        "end": {
          "line": 30,
          "col": 41
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The local name c can only be given to an existential variable
  introduced by this GADT constructor. The type annotation tries to bind it to
  the name a that was defined before."
      },
      {
        "start": {
          "line": 30,
          "col": 46
        },
        "end": {
          "line": 30,
          "col": 47
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound value f"
      },
      {
        "start": {
          "line": 30,
          "col": 48
        },
        "end": {
          "line": 30,
          "col": 49
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound value x"
      },
      {
        "start": {
          "line": 32,
          "col": 17
        },
        "end": {
          "line": 32,
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
