See https://github.com/ocaml/ocaml-lsp/issues/2143


  $ cat >issue_example.ml <<'EOF'
  > let oneof_array xs st = Array.get xs (Random.State.int st (Array.length xs))
  > 
  > let array_of_set (type t) (module S: Set.S with type t = t) set =
  >   set |> S.to_seq |> Array.of_seq |> oneof_array
  > EOF

  $ $MERLIN single errors -filename issue_example.ml -warn-error +5 <issue_example.ml
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
          "col": 48
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type Random.State.t -> S.elt
  but an expression was expected of type 'a
  The type constructor S.elt would escape its scope"
      },
      {
        "start": {
          "line": 4,
          "col": 2
        },
        "end": {
          "line": 4,
          "col": 48
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Error (warning 5): this function application is partial,
  maybe some arguments are missing."
      }
    ],
    "notifications": []
  }

2. Type error and deprecation alert at the exact same location:

  $ cat >type_and_alert.ml <<'EOF'
  > module M : sig
  >   val f : unit -> unit [@@ocaml.deprecated "dep"]
  > end = struct
  >   let f () = ()
  > end
  > let x : int = M.f
  > EOF

  $ $MERLIN single errors -filename type_and_alert.ml <type_and_alert.ml
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
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type unit -> unit but an expression was expected of type
    int"
      },
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

3. Multiple alerts at the exact same location:

  $ cat >multiple_alerts.ml <<'EOF'
  > module M : sig
  >   val f : unit -> unit [@@ocaml.deprecated "dep"] [@@ocaml.alert myalert "alrt"]
  > end = struct
  >   let f () = ()
  > end
  > let () = M.f ()
  > EOF

  $ $MERLIN single errors -filename multiple_alerts.ml <multiple_alerts.ml
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
      },
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
        "message": "Alert myalert: M.f
  alrt"
      }
    ],
    "notifications": []
  }
