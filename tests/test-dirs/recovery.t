_ should be parsed as Pexp_hole or Pmod_hole, and shouldn't be treated as a type
error.

  $ echo "let () = _" | \
  > $MERLIN single errors -filename hole_0.ml
  {
    "class": "return",
    "value": [],
    "notifications": []
  }

  $ echo "module M = _" | \
  > $MERLIN single errors -filename hole_2.ml
  {
    "class": "return",
    "value": [],
    "notifications": []
  }

This incomplete expression should generate only a parser error.
The hole is filled with merlin.hole.

  $ echo "let _ =" | \
  > $MERLIN single errors -filename hole_1.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 0
        },
        "end": {
          "line": 2,
          "col": 0
        },
        "type": "parser",
        "sub": [],
        "valid": true,
        "message": "Syntax error, expecting fun_expr"
      }
    ],
    "notifications": []
  }

  $ echo "let _ =" | \
  > $MERLIN single dump -what source -filename hole_1.ml | \
  > tr -d '\n' | jq '.value'
  "let _ = _"

  $ echo "module M : sig val f : int -> unit end =" |
  > $MERLIN single errors -filename "module_recovery.ml"
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 40
        },
        "end": {
          "line": 1,
          "col": 40
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Signature mismatch:
  Modules do not match: sig end is not included in sig val f : int -> unit end
  The value f is required but not provided
  File \"module_recovery.ml\", line 1, characters 15-34: Expected declaration"
      },
      {
        "start": {
          "line": 2,
          "col": 0
        },
        "end": {
          "line": 2,
          "col": 0
        },
        "type": "parser",
        "sub": [],
        "valid": true,
        "message": "Syntax error, expecting module_expr"
      }
    ],
    "notifications": []
  }

  $ echo "module M =" |
  > $MERLIN single dump -what source -filename "module_recovery.ml"
  {
    "class": "return",
    "value": "module M = struct  end
  
  ",
    "notifications": []
  }

A bit trickier: the recovery is tempted to put a ->. (unreachable), but the
penalty should prevent it.

  $ echo "let f = function _ ->" | \
  > $MERLIN single errors -filename  "hole_2.ml"
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 0
        },
        "end": {
          "line": 2,
          "col": 0
        },
        "type": "parser",
        "sub": [],
        "valid": true,
        "message": "Syntax error, expecting fun_expr"
      }
    ],
    "notifications": []
  }

Issue #713: merlin would error when it cannot recover, but in some files there
really is nothing to recover.

Note: in 4.08 the suggestion changed from "exception" to "open" and the recovery
generates "let open struct end in [%merlin.hole ]".

FIXME: the syntax error message is off the mark.

  $ echo "let" | \
  > $MERLIN single errors -filename  "two_constr.ml" | \
  > sed 's/expecting.*/<unstable suggestion>/'
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 0
        },
        "end": {
          "line": 2,
          "col": 0
        },
        "type": "parser",
        "sub": [],
        "valid": true,
        "message": "Syntax error, <unstable suggestion>
      }
    ],
    "notifications": []
  }

  $ echo "let test x = match x with | None -> exit 1 | Some pkg -> pkg end" | \
  > $MERLIN single dump -what source -filename "lessminus.ml"
  {
    "class": "return",
    "value": "let test x =
    match x with
    | None -> ((exit 1)[@merlin.loc ])
    | Some pkg -> ((pkg)[@merlin.loc ])
  
  ",
    "notifications": []
  }
