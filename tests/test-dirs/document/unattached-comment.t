A test showing that we manage to get docstrings even when they are not kept as
attributes on the AST.

  $ cat >test.ml <<EOF
  > let foo x y = (** incorrect doc for foo *)
  >   x + y
  >  
  > let bar = foo
  > EOF

  $ $MERLIN single document -position 4:13 -filename test.ml < test.ml
  {
    "class": "return",
    "value": "incorrect doc for foo",
    "notifications": []
  }

And that it also works outside of the current buffer:

  $ $OCAMLC -c -bin-annot -w +50 test.ml
  File "test.ml", line 1, characters 14-42:
  1 | let foo x y = (** incorrect doc for foo *)
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning 50 [unexpected-docstring]: unattached documentation comment (ignored)

  $ $MERLIN single document -position 1:18 -filename outside.ml << EOF
  > let bar = Test.foo
  > EOF
  {
    "class": "return",
    "value": "incorrect doc for foo",
    "notifications": []
  }
