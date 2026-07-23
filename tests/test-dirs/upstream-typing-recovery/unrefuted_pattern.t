  $ cat >unrefuted_pattern.ml <<'EOF'
  > type ('env, 'a) var =
  >  | Zero : ('a * 'env, 'a) var
  >  | Succ : ('env, 'a) var -> ('b * 'env, 'a) var
  > 
  > type ('env, 'a) typ =
  >  | Tint : ('env, int) typ
  >  | Tbool : ('env, bool) typ
  >  | Tvar : ('env, 'a) var -> ('env, 'a) typ
  > 
  > type _ t =
  >   | Int : int t
  >   | Bool : bool t
  > 
  > let deep : (char t * int) option -> char = function
  >   | None -> 'c'
  >   | _ -> .
  > 
  > let f : type env a. (env, a) typ -> (env, a) typ -> int = fun ta tb ->
  >  match ta, tb with
  >    | Tint, Tint -> 0
  >    | Tbool, Tbool -> 1
  >    | Tvar var, tb -> 2
  >    | _ -> .   (* error *)
  > 
  > let x = (f Tint Tint) + 15 + (Char.code (deep None))
  > let u : string = x
  > EOF

  $ $MERLIN single errors -filename unrefuted_pattern.ml < unrefuted_pattern.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 23,
          "col": 5
        },
        "end": {
          "line": 23,
          "col": 6
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This match case could not be refuted.
  Here is an example of a value that would reach it: (Tint, Tvar Zero)"
      },
      {
        "start": {
          "line": 26,
          "col": 17
        },
        "end": {
          "line": 26,
          "col": 18
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The value x has type int but an expression was expected of type string"
      }
    ],
    "notifications": []
  }
