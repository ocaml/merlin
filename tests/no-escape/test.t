These tests ensure that all type errors are caught by the kernel, no exception
should reach top-level

  $ echo "type p = P : 'a -> 'a -> p" | \
  > $MERLIN single errors -filename ./incorrect_gadt.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 19
        },
        "end": {
          "line": 1,
          "col": 26
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The type constructor p expects 0 argument(s),
         but is here applied to 1 argument(s)"
      },
      {
        "start": {
          "line": 1,
          "col": 22
        },
        "end": {
          "line": 1,
          "col": 24
        },
        "type": "parser",
        "sub": [],
        "valid": true,
        "message": "Syntax error"
      }
    ],
    "notifications": []
  }

  $ echo "let error : unknown_type_constructor = assert false" | \
  > $MERLIN single errors -filename  "unkown_constr.ml"
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 12
        },
        "end": {
          "line": 1,
          "col": 36
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound type constructor unknown_type_constructor"
      }
    ],
    "notifications": []
  }

  $ echo "val error : unknown_type_constructor" | \
  > $MERLIN single errors -filename  "unkown_constr.mli"
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 12
        },
        "end": {
          "line": 1,
          "col": 36
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound type constructor unknown_type_constructor"
      }
    ],
    "notifications": []
  }

  $ echo "type t = A | A" | \
  > $MERLIN single errors -filename  "two_constr.ml"
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 1,
          "col": 14
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Two constructors are named A"
      }
    ],
    "notifications": []
  }

  $ echo "type t = A | A" | \
  > $MERLIN single errors -filename  "two_constr.mli"
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 1,
          "col": 14
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Two constructors are named A"
      }
    ],
    "notifications": []
  }

  $ echo "let x = 4 val x : int" | \
  > $MERLIN single errors -filename  "ml_in_mli.mli"
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 1,
          "col": 3
        },
        "type": "parser",
        "sub": [],
        "valid": true,
        "message": "Syntax error"
      }
    ],
    "notifications": []
  }

vals are no allowed in ml files and detected during semantic analysis

  $ echo "val x : int" | \
  > $MERLIN single errors -filename  "mli_in_ml.ml"
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 1,
          "col": 11
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Value declarations are only allowed in signatures"
      }
    ],
    "notifications": []
  }

The code should raise a single error (for Bb typo), but shouldn't report the
unused case after

  $ $MERLIN single errors -filename "unused_case_after_error.ml" <<EOF \
  > type t = A | B | C \
  > let f = function \
  >   | A -> 1 \
  >   | Bb -> 1 \
  >   | C -> 1 \
  > EOF
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
          "col": 6
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This variant pattern is expected to have type t
         The constructor Bb does not belong to type t"
      }
    ],
    "notifications": []
  }

