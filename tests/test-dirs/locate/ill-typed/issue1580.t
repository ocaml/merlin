Issue #1580:

  $ cat >test.ml << 'EOF'
  > module type S = sig
  >   val foo : unit -> ('a -> 'a -> bool) -> unit
  > end
  > 
  > module F (M : S) = struct
  >   let z () = M.foo () compare
  > end
  > EOF

  $ $MERLIN single errors -filename test.ml <test.ml 
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 6,
          "col": 22
        },
        "end": {
          "line": 6,
          "col": 29
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The value compare has type 'a -> 'a -> int
  but an expression was expected of type 'a -> 'a -> bool
  Type int is not compatible with type bool"
      }
    ],
    "notifications": []
  }

The typing recovery allows Merlin to perform the correct jump here:

  $ $MERLIN single locate -position 6:16 \
  > -filename test.ml <test.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/test.ml",
    "pos": {
      "line": 2,
      "col": 6
    }
  }


  $ $MERLIN single locate -position 6:25 \
  > -filename test.ml <test.ml | jq '.value'
  {
    "file": "lib/ocaml/stdlib.mli",
    "pos": {
      "line": 158,
      "col": 9
    }
  }

Issue #1588:

  $ cat >test.ml <<'EOF'
  > let test ~f:(_ : unit -> unit) = ()
  > type t = F : { f : unit -> 'fn } -> t
  > let call (F { f }) = test ~f
  > EOF

  $ $MERLIN single locate -position 3:23 \
  > -filename test.ml <test.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/test.ml",
    "pos": {
      "line": 1,
      "col": 4
    }
  }

  $ $MERLIN single errors -filename test.ml <test.ml 
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 27
        },
        "end": {
          "line": 3,
          "col": 28
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The value f has type unit -> $fn but an expression was expected of type
    unit -> unit
  Type $fn is not compatible with type unit
  Hint: $fn is an existential type bound by the constructor F."
      }
    ],
    "notifications": []
  }
