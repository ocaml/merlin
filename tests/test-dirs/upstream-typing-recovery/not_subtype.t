  $ cat >not_subtype.ml <<'EOF'
  > (* The expansion is a bit disappointing and less precise than when
  >    recovery mode is not active. *)
  > 
  > type u0 = int * int -> unit
  > type u = u0 -> unit
  > type v0 = float * float -> unit
  > type v = v0 -> unit
  > let f = (g: u -> unit :> v -> unit)
  > 
  > class a = object
  >   method foo x = x + 1
  > end
  > 
  > class b = object
  >   method foo x = x - 1
  >   method bar x = x + 1
  > end
  > 
  > let f x = (x: b :> a)
  > 
  > let g x =
  >   let g x = (x: a :> b) in
  >   (g x) # foo + 1
  > 
  > let t : string = 10
  > EOF

  $ $MERLIN single errors -filename not_subtype.ml < not_subtype.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 8,
          "col": 8
        },
        "end": {
          "line": 8,
          "col": 35
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Type ((int * int -> unit) -> unit) -> unit is not a subtype of
    ((float * float -> unit) -> unit) -> unit
  Type float is not a subtype of int"
      },
      {
        "start": {
          "line": 22,
          "col": 12
        },
        "end": {
          "line": 22,
          "col": 23
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Type < foo : int -> int > is not a subtype of
    < bar : int -> int; foo : int -> int >
  The first object type has no method bar"
      },
      {
        "start": {
          "line": 25,
          "col": 17
        },
        "end": {
          "line": 25,
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
