Testing the behavior of custom operators

  $ cat >main.ml <<EOF
  > let ( := ) v a = Printf.printf "%s = %d;\n" v a
  > let () = "foo" := 3
  > let () = ( := ) "foo"  3
  > EOF

  $ $MERLIN single locate -look-for ml -position 2:17 \
  > -filename ./main.ml < ./main.ml | jq '.value.pos'
  {
    "line": 1,
    "col": 4
  }

  $ $MERLIN single locate -look-for ml -position 3:12 \
  > -filename ./main.ml < ./main.ml | jq '.value.pos'
  {
    "line": 1,
    "col": 4
  }

Testing custom indexing operators

  $ cat >main.ml <<EOF
  > let (.%{;..}) a k = Printf.printf "%s.coeffRef(%d);\n" a k.(0)
  > let (.%{ }) a k = Printf.printf "%s.coeffRef(%d);\n" a k
  > let name = "baz"
  > let () = name.%{2;4}
  > let () = name.%{5}
  > let () = ( .%{ } ) name 3
  > EOF

  $ $MERLIN single locate -look-for ml -position 4:15 \
  > -filename ./main.ml < ./main.ml | jq '.value'
  "Not in environment '%'"

  $ $MERLIN single locate -look-for ml -position 4:16 \
  > -filename ./main.ml < ./main.ml | jq '.value'
  "Not a valid identifier"

  $ $MERLIN single locate -look-for ml -position 5:15 \
  > -filename ./main.ml < ./main.ml | jq '.value'
  "Not in environment '%'"

  $ $MERLIN single locate -look-for ml -position 5:15 \
  > -filename ./main.ml < ./main.ml | jq '.value'
  "Not in environment '%'"

  $ $MERLIN single locate -look-for ml -position 5:16 \
  > -filename ./main.ml < ./main.ml | jq '.value'
  "Not a valid identifier"

  $ $MERLIN single locate -look-for ml -position 6:13 \
  > -filename ./main.ml < ./main.ml | jq '.value'
  "Not in environment '%'"

  $ $MERLIN single locate -look-for ml -position 6:14 \
  > -filename ./main.ml < ./main.ml | jq '.value'
  "Not a valid identifier"

  $ $MERLIN single locate -look-for ml -position 6:15 \
  > -filename ./main.ml < ./main.ml | jq '.value'
  "Not a valid identifier"
