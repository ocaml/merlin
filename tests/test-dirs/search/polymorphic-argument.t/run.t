  $ cat >poly.ml <<'EOF'
  > let apply_poly (g : 'a. 'a -> 'a) = (g 0, g "x")
  > EOF

  $ $OCAMLC -c poly.ml

  $ cat >context.ml <<'EOF'
  > let () = ()
  > EOF

search-by-type finds the function:
  $ $MERLIN single search-by-type -filename ./context.ml \
  > -position 1:9 -limit 30 -query "('a -> 'a) -> int * string" |
  > tr '\n' ' ' | jq -c '.value[] | {name,type,cost}' | grep Poly
  {"name":"Poly.apply_poly","type":"('a. 'a -> 'a) -> int * string","cost":0}

search-by-polarity finds it as well:
  $ $MERLIN single search-by-polarity -filename ./context.ml \
  > -position 1:9 -query "+int +string" |
  > tr '\n' ' ' | jq -c '.value.entries[] | {name,desc}' | grep Poly
  {"name":"Poly.apply_poly","desc":"('a. 'a -> 'a) -> int * string"}
