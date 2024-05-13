When going to the definition of a module defined by a functor, merlin jumps
straight to the functor.

  $ cat > func.ml <<EOF
  > module Make () = struct
  >   let u = ()
  > end
  > 
  > module T = Make ();;
  > 
  > let () = T.u
  > EOF

  $ $MERLIN single locate -look-for ml -position 7:11 -filename ./func.ml < ./func.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/func.ml",
      "pos": {
        "line": 2,
        "col": 6
      }
    },
    "notifications": []
  }
  $ $MERLIN single locate -look-for ml -position 7:9 -filename ./func.ml < ./func.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/func.ml",
      "pos": {
        "line": 5,
        "col": 7
      }
    },
    "notifications": []
  }
