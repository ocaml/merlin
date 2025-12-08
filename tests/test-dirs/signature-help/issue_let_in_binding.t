  $ cat > test1.ml <<'EOF'
  > let _ =
  > let f = List.map   in
  > let _ = 5 in
  > 7
  > EOF

  $ $MERLIN single signature-help -position 2:17 -filename test1 < test1.ml | jq '.value.signatures[0].label'
  "List.map : ('a -> 'b) -> 'a list -> 'b list"

FIXME: Signature-help does not trigger on unfinished let ... in bindings.
  $ cat > test2.ml <<'EOF'
  > let _ =
  > let f = List.map  
  > let _ = 5 in
  > 7
  > EOF

  $ $MERLIN single signature-help -position 2:17 -filename test2 < test2.ml | jq '.value.signatures[0].label'
  null
