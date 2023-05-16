
  $ cat >test.ml <<EOF
  > let goo = object
  >   method bar i = i + 5
  >   method bazs _k = "hello"
  > end
  > 
  > let something = go
  > EOF

We complete the name of the object

  $ $MERLIN single complete-prefix -position 6:18 -prefix "go" \
  > -filename test.ml < test.ml | jq '.value.entries'
  [
    {
      "name": "goo",
      "kind": "Value",
      "desc": "< bar : int -> int; bazs : 'a -> string >",
      "info": "",
      "deprecated": false
    }
  ]


  $ cat >test.ml <<EOF
  > let foo = object
  >   method bar i = i + 5
  >   method bazs _k = "hello"
  > end
  > 
  > let something = foo#
  > EOF

After a # we complete methods names

  $ $MERLIN single complete-prefix -position 6:20 -prefix "" \
  > -filename test.ml < test.ml | jq '.value.entries'
  [
    {
      "name": "bar",
      "kind": "#",
      "desc": "int -> int",
      "info": "",
      "deprecated": false
    },
    {
      "name": "bazs",
      "kind": "#",
      "desc": "'_weak1 -> string",
      "info": "",
      "deprecated": false
    }
  ]

  $ cat >test.ml <<EOF
  > let foo = object
  >   method bar i = i + 5
  >   method bazs _k = "hello"
  > end
  > 
  > let something = foo#baz
  > EOF

And filtering works with methods names

  $ $MERLIN single complete-prefix -position 6:23 -prefix "baz" \
  > -log-file - -log-section Completion \
  > -filename test.ml < test.ml | jq '.value.entries'
  [
    {
      "name": "bazs",
      "kind": "#",
      "desc": "'_weak1 -> string",
      "info": "",
      "deprecated": false
    }
  ]

It also works when inside modules

  $ cat >test.ml <<EOF
  > module A = struct
  >   let foo = object
  >     method bar i = i + 5
  >     method bazs _k = "hello"
  >   end
  > end
  > 
  > let something = A.foo#ba
  > EOF

  $ $MERLIN single complete-prefix -position 8:24 -prefix "ba" \
  > -filename test.ml < test.ml | jq '.value.entries'
  [
    {
      "name": "bar",
      "kind": "#",
      "desc": "int -> int",
      "info": "",
      "deprecated": false
    },
    {
      "name": "bazs",
      "kind": "#",
      "desc": "'_weak1 -> string",
      "info": "",
      "deprecated": false
    }
  ]
