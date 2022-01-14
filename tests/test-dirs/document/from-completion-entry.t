  $ cat >test.ml <<EOF
  > let _ = Test2.
  > let _ = Test3mlonly.
  > 
  > (** doc for samefile *)
  > let samefile = 42
  > let _ = sam
  > EOF

  $ cat >test2.ml <<EOF
  > let foo = 42
  > EOF

  $ cat >test2.mli <<EOF
  > (** doc of Test2.foo *)
  > val foo : int
  > EOF

  $ cat >test3mlonly.ml <<EOF
  > (** doc of Test3mlonly.bar *)
  > let bar = 42
  > EOF

  $ $OCAMLC -bin-annot -c test2.mli test2.ml
  $ $OCAMLC -bin-annot -c test3mlonly.ml

  $ cat >.merlin <<EOF
  > S .
  > B.
  > EOF

  $ $MERLIN single complete-prefix -position 1:14 -prefix Test2. -doc y \
  > -filename test.ml < test.ml
  {
    "class": "return",
    "value": {
      "entries": [
        {
          "name": "foo",
          "kind": "Value",
          "desc": "int",
          "info": "doc of Test2.foo",
          "deprecated": false
        }
      ],
      "context": null
    },
    "notifications": []
  }

  $ $MERLIN single complete-prefix -position 2:20 -prefix Test3mlonly. -doc y \
  > -filename test.ml < test.ml
  {
    "class": "return",
    "value": {
      "entries": [
        {
          "name": "bar",
          "kind": "Value",
          "desc": "int",
          "info": "doc of Test3mlonly.bar",
          "deprecated": false
        }
      ],
      "context": null
    },
    "notifications": []
  }

  $ $MERLIN single complete-prefix -position 6:11 -prefix sam -doc y \
  > -filename test.ml < test.ml
  {
    "class": "return",
    "value": {
      "entries": [
        {
          "name": "samefile",
          "kind": "Value",
          "desc": "int",
          "info": "doc for samefile",
          "deprecated": false
        }
      ],
      "context": null
    },
    "notifications": []
  }
