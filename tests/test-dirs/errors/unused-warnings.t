  $ cat >test.ml <<EOF
  > open Printf
  > 
  > module type X = sig end
  > 
  > let x =
  >   let y = 42 in
  >   ()
  > 
  > let _f x = ()
  > 
  > type x = {
  >   foo: int }
  > type y =
  >   | Foo
  > 
  > let () = for i = 0 to 0 do () done
  > EOF

FIXME: It is expected that unused warnings are not shown but the result is
incoherent.
  $ $MERLIN single errors -filename test.ml <test.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 6,
          "col": 6
        },
        "end": {
          "line": 6,
          "col": 7
        },
        "type": "warning",
        "sub": [],
        "valid": true,
        "message": "Warning 26: unused variable y."
      }
    ],
    "notifications": []
  }
