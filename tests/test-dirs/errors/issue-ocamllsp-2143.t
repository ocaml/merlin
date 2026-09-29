See https://github.com/ocaml/ocaml-lsp/issues/2143

  $ $MERLIN single errors -filename test.ml <<EOF
  > module M : sig
  >   val f : unit -> unit [@@ocaml.deprecated "dep"]
  > end = struct
  >   let f () = ()
  > end
  > let x : int = M.f
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 6,
          "col": 14
        },
        "end": {
          "line": 6,
          "col": 17
        },
        "type": "warning",
        "sub": [],
        "valid": true,
        "message": "Alert deprecated: M.f
  dep"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single errors -filename alerts.ml <<EOF
  > module M : sig
  >   val f : unit -> unit [@@ocaml.deprecated "dep"] [@@ocaml.alert myalert "alrt"]
  > end = struct
  >   let f () = ()
  > end
  > let () = M.f ()
  > EOF
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
          "col": 12
        },
        "type": "warning",
        "sub": [],
        "valid": true,
        "message": "Alert deprecated: M.f
  dep"
      }
    ],
    "notifications": []
  }
