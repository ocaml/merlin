Initially from issue https://github.com/ocaml/merlin/issues/1125

  $ cat > gadt.ml <<EOF
  > 
  > type _ term =
  >   | Int : int -> int term
  >   | Pair : 'a term * 'b term -> ('a * 'b) term
  >   | Fst : ('a * 'b) term -> 'a term
  >   | Snd : ('a * 'b) term -> 'b term
  > 
  > let rec eval : type a . a term -> a = function
  >   | Int n -> n
  >   | Pair (a, b) -> eval a, eval b
  >   | Fst p -> fst (eval p)
  >   | Snd p -> snd (eval p)
  > EOF

  $ $MERLIN single type-enclosing -position 3:5 -verbosity 0 \
  > -filename ./gadt.ml < ./gadt.ml | tr '\r\n' ' ' | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 3,
        "col": 4
      },
      "end": {
        "line": 3,
        "col": 7
      },
      "type": "(module Stdlib__Int)",
      "tail": "no"
    },
    {
      "start": {
        "line": 2,
        "col": 0
      },
      "end": {
        "line": 6,
        "col": 35
      },
      "type": "type _ term =     Int : int -> int term   | Pair : 'a term * 'b term -> ('a * 'b) term   | Fst : ('a * 'b) term -> 'a term   | Snd : ('a * 'b) term -> 'b term",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 9:5 -verbosity 0 \
  > -filename ./gadt.ml < ./gadt.ml | tr '\r\n' ' ' | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 9,
        "col": 4
      },
      "end": {
        "line": 9,
        "col": 7
      },
      "type": "int -> int term",
      "tail": "no"
    },
    {
      "start": {
        "line": 9,
        "col": 4
      },
      "end": {
        "line": 9,
        "col": 9
      },
      "type": "a term",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 8:21 -verbosity 0 \
  > -filename ./gadt.ml < ./gadt.ml | tr '\r\n' ' ' | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 8,
        "col": 8
      },
      "end": {
        "line": 12,
        "col": 25
      },
      "type": "'a term -> 'a",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 8:9 -verbosity 0 \
  > -filename ./gadt.ml < ./gadt.ml | tr '\r\n' ' ' | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 8,
        "col": 8
      },
      "end": {
        "line": 8,
        "col": 12
      },
      "type": "'a. 'a term -> 'a",
      "tail": "no"
    }
  ]
