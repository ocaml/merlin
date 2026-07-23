  $ cat >undefined_method.ml <<'EOF'
  > class type t = object
  >   method foo : int -> unit
  >   method bar : int
  > end
  > 
  > let f (x : t) : unit * int * string * string =
  >   let a = x # foo 10 in
  >   let b = x # bar in
  >   let c = x # baz and y = x # foobar in
  >   (a, b, c, y)
  > 
  > (* Should work *)
  > let f (x : #t) : unit * int * string * string =
  >   let a = x # foo 10 in
  >   let b = x # bar in
  >   let c = x # baz and y = x # foobar in
  >   (a, b, c, y)
  > 
  > let _ =
  >   let a = object
  >     method foo = 10
  >   end in
  >   let b = (a # bar) + (a # foo) in
  >   b + 12
  > 
  > let t : int = true
  > EOF

  $ $MERLIN single errors -filename undefined_method.ml < undefined_method.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 9,
          "col": 10
        },
        "end": {
          "line": 9,
          "col": 11
        },
        "type": "typer",
        "sub": [
          {
            "start": {
              "line": 9,
              "col": 10
            },
            "end": {
              "line": 9,
              "col": 11
            },
            "message": "Hint: Did you mean bar?"
          }
        ],
        "valid": true,
        "message": "This expression has type < bar : int; foo : int -> unit >
  It has no method baz"
      },
      {
        "start": {
          "line": 9,
          "col": 26
        },
        "end": {
          "line": 9,
          "col": 27
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type < bar : int; foo : int -> unit >
  It has no method foobar"
      },
      {
        "start": {
          "line": 23,
          "col": 11
        },
        "end": {
          "line": 23,
          "col": 12
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type < foo : int >
  It has no method bar"
      },
      {
        "start": {
          "line": 26,
          "col": 14
        },
        "end": {
          "line": 26,
          "col": 18
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression should not be a boolean literal, the expected type is int"
      }
    ],
    "notifications": []
  }
