  $ cat >main.ml <<EOF
  > type foo_record = { foo : int }
  > 
  > type foo = Foo of foo_record
  > type bar = Bar of { bar : int }
  > 
  > let x : foo = _ (* succeeds *)
  > let y : bar = _ (* fails *)
  > EOF

  $ $MERLIN single construct -position 6:15 \
  > -filename main.ml <main.ml | jq '.value[1]'
  [
    "(Foo _)"
  ]

Construct also works with inline records
  $ $MERLIN single construct -position 7:15 \
  > -filename main.ml <main.ml | jq '.value[1]'
  [
    "(Bar _)"
  ]
