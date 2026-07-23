  $ cat >inside_check.ml <<'EOF'
  > type (_, _) eq = Eq : ('a, 'a) eq
  > 
  > let bad : type a. ?opt:(a, int -> int -> int) eq -> unit -> a =
  >   fun ?opt:(Eq = assert false) () x y -> x + y
  > 
  > let x:int = "hello"
  > EOF

  $ $MERLIN single errors -filename inside_check.ml < inside_check.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 2
        },
        "end": {
          "line": 4,
          "col": 46
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The syntactic arity of the function doesn't match the type constraint:
  This function has 4 syntactic arguments, but its type is constrained to
    ?opt:(a, int -> int -> int) eq -> unit -> a.
   Hint: consider splitting the function definition into
     fun ... gadt_pat -> fun ...
     where gadt_pat is the pattern with the GADT constructor that
     introduces the local type equation on a."
      },
      {
        "start": {
          "line": 6,
          "col": 12
        },
        "end": {
          "line": 6,
          "col": 19
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This constant has type string but an expression was expected of type int"
      }
    ],
    "notifications": []
  }
