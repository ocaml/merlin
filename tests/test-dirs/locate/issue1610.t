  $ cat >main.ml <<EOF
  > module type T = sig
  >   type 'a t
  > end
  > 
  > module M (T : T) = struct
  >   type t = int T.t
  > end
  > 
  > module T = struct type 'a t end
  > 
  > type t = M(T).t
  > EOF

FIXME: we should jump to the functor's body, not the current definition
  $ $MERLIN single locate -look-for ml -position 11:15 \
  > -filename main.ml <main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/main.ml",
    "pos": {
      "line": 11,
      "col": 0
    }
  }
