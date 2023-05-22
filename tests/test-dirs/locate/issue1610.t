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
This is due to an issue with identifier-reconstruction
  $ $MERLIN single locate -look-for ml -position 11:15 \
  > -filename main.ml <main.ml 
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/main.ml",
      "pos": {
        "line": 11,
        "col": 0
      }
    },
    "notifications": []
  }

It works as expected when the user inputs the expression manually
  $ $MERLIN single locate -prefix 'M(T).t' -look-for ml -position 11:15 \
  > -filename main.ml <main.ml 
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/main.ml",
      "pos": {
        "line": 6,
        "col": 2
      }
    },
    "notifications": []
  }
