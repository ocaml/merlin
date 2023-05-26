  $ cat >main.ml <<EOF
  > let foo : [< \`Foo ] option = None
  > 
  > let () =
  >   match foo with
  >   | None | Some _ -> ()
  > EOF

  $ $MERLIN single case-analysis -start 5:16 -end 5:16 \
  > -filename main.ml <main.ml |
  > jq '.value[1]'
  "`Foo"

  $ cat >main.ml <<EOF
  > let foo : [> \`Foo ] option = None
  > 
  > let () =
  >   match foo with
  >   | None | Some _ -> ()
  > EOF

  $ $MERLIN single case-analysis -start 5:16 -end 5:16 \
  > -filename main.ml <main.ml |
  > jq '.value[1]'
  "`Foo"

  $ cat >main.ml <<EOF
  > let foo : [< \`Foo | \`Bar > \`Foo] option = None
  > 
  > let () =
  >   match foo with
  >   | None | Some _ -> ()
  > EOF

  $ $MERLIN single case-analysis -start 5:16 -end 5:16 \
  > -filename main.ml <main.ml |
  > jq '.value[1]'
  "None | Some `Bar | Some `Foo"
