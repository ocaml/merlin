  $ cat >function_arity_type_clash.ml <<'EOF'
  > type (_, _) eq = Eq : ('a, 'a) eq
  > 
  > let bad : type a. ?opt:(a, int -> int) eq -> unit -> a =
  >   fun ?opt:(Eq = assert false) () x -> x + 1
  > 
  > let x : string = 10
  > EOF

  $ $MERLIN single errors -filename function_arity_type_clash.ml < function_arity_type_clash.ml
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
          "col": 44
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The syntactic arity of the function doesn't match the type constraint:
  This function has 3 syntactic arguments, but its type is constrained to
    ?opt:(a, int -> int) eq -> unit -> a.
   Hint: consider splitting the function definition into
     fun ... gadt_pat -> fun ...
     where gadt_pat is the pattern with the GADT constructor that
     introduces the local type equation on a."
      },
      {
        "start": {
          "line": 6,
          "col": 17
        },
        "end": {
          "line": 6,
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
