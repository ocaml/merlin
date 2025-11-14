Ensure that the issue highlighted in issue #1517 is no longer relevant.
See https://github.com/ocaml/merlin/issues/1517.

  $ cat >main.ml <<EOF
  > type _ bar =
  >   | Flag : int bar
  >   | Other : 'a bar
  > 
  > let foo : unit -> int bar -> unit =
  >   fun () Other -> ()
  > EOF

  $ $MERLIN single errors -filename main.ml < main.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 6,
          "col": 9
        },
        "end": {
          "line": 6,
          "col": 14
        },
        "type": "warning",
        "sub": [],
        "valid": true,
        "message": "Warning 8: this pattern-matching is not exhaustive.
    Here is an example of a case that is not matched: Flag"
      }
    ],
    "notifications": []
  }
