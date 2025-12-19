  $ cat > test.ml <<EOF
  > module rec M : sig
  >   val f : unit -> unit
  > end = struct
  >   let f () = ()
  > end
  > and N : sig
  >   val foo : unit -> unit
  > end = struct
  >   let foo () = M.f ()
  > end
  > let foo () = M.f ()
  > EOF

  $ ocamlmerlin single locate -position 11:16 -look-for implementation -filename test.ml < test.ml | jq .value
  {
    "file": "$TESTCASE_ROOT/test.ml",
    "pos": {
      "line": 4,
      "col": 6
    }
  }

-- FIXME : Merlin should return the definition position instead of the declaration one
  $ ocamlmerlin single locate -position 9:18 -look-for implementation -filename test.ml < test.ml | jq .value
  {
    "file": "$TESTCASE_ROOT/test.ml",
    "pos": {
      "line": 2,
      "col": 6
    }
  }
