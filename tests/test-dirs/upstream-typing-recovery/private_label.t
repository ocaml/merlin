  $ cat >private_label.ml <<'EOF'
  > type t =  private {  mutable foo: int}
  > 
  > let f (x : t) =
  >   x.foo <- 10
  > 
  > type e = private A of { mutable bar : int}
  > 
  > let e x y =
  >   (* Should fail *)
  >   let () = x.foo <- y in
  > 
  >   (* Should work *)
  >   let () = (f x) in
  >   10
  > 
  > let g = function
  >   | A x -> x.bar <- 10
  > EOF

  $ $MERLIN single errors -filename private_label.ml < private_label.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 4
        },
        "end": {
          "line": 4,
          "col": 7
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Cannot assign field foo of the private type t"
      },
      {
        "start": {
          "line": 10,
          "col": 13
        },
        "end": {
          "line": 10,
          "col": 16
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Cannot assign field foo of the private type t"
      },
      {
        "start": {
          "line": 17,
          "col": 13
        },
        "end": {
          "line": 17,
          "col": 16
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Cannot assign field bar of the private type e.A"
      }
    ],
    "notifications": []
  }
