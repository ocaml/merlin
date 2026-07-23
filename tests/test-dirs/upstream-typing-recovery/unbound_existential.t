  $ cat >unbound_existential.ml <<'EOF'
  > type _ ty = Int : int ty
  > type dyn = Dyn : 'a ty * 'a -> dyn
  > type u = C : 'a * ('a -> 'b list) -> u
  > 
  > let a = function Dyn (type a) (w, x : _) -> 100
  > let b = function C (type a) (x, f : a * (a -> a list)) ->
  >   let () = ignore (x : a) in
  >   200
  > 
  > let c = (a (Dyn (Int, 10))) + (b (C (10, (fun _ -> []))))
  > let t : string = c
  > EOF

  $ $MERLIN single errors -filename unbound_existential.ml < unbound_existential.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 5,
          "col": 38
        },
        "end": {
          "line": 5,
          "col": 39
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This type does not bind all existentials in the constructor:
    type a. 'a ty * 'a"
      },
      {
        "start": {
          "line": 6,
          "col": 36
        },
        "end": {
          "line": 6,
          "col": 53
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This type does not bind all existentials in the constructor:
    type a. a * (a -> a list)"
      },
      {
        "start": {
          "line": 7,
          "col": 19
        },
        "end": {
          "line": 7,
          "col": 20
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound value x"
      },
      {
        "start": {
          "line": 11,
          "col": 17
        },
        "end": {
          "line": 11,
          "col": 18
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The value c has type int but an expression was expected of type string"
      }
    ],
    "notifications": []
  }
