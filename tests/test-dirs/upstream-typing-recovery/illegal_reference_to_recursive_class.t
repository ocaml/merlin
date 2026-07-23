  $ cat >illegal_reference_to_recursive_class.ml <<'EOF'
  > module rec Foo : sig class type c = object method x : int end end = Foo
  > and Bar : sig class type c = object inherit Foo.c end end = Bar
  > and Baz : sig class type c = object inherit Bar.c end end = Baz;;
  > EOF

  $ $MERLIN single errors -filename illegal_reference_to_recursive_class.ml < illegal_reference_to_recursive_class.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 44
        },
        "end": {
          "line": 2,
          "col": 47
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This module type is recursive. This use of the recursive module Foo
  within the definition of the module Bar
  makes the module type of Bar depend on the module type of Foo.
  Such recursive definitions of module types are not allowed."
      }
    ],
    "notifications": []
  }
