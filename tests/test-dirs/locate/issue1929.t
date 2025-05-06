  $ cat >test.ml <<'EOF'
  > module Foo = struct
  >   type t =
  >     { foo : int
  >     ; bar : int
  >     }
  > 
  >   let foo = "hello"
  > end
  > 
  > let _ =
  >   let foo = 10 in
  >   let bar = 10 in
  >   ({ Foo.foo; bar } : Foo.t)
  > ;;
  > EOF

FIXME: this answer is wrong. The correct value jumping to `foo` line 11
  $ $MERLIN single locate -position 13:10 \
  > -filename test.ml <test.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/test.ml",
      "pos": {
        "line": 7,
        "col": 6
      }
    },
    "notifications": []
  }
