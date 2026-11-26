  $ $MERLIN server stop-server

  $ cat > test.ml <<EOF
  > let x = 1
  > let y = x + 1
  > EOF

  $ cat > commands.txt <<EOF
  > ocamlmerlin server errors -filename test.ml < test.ml
  > ocamlmerlin server type-enclosing -position '2:8' -index 0 -filename test.ml < test.ml
  > ocamlmerlin server locate -look-for ml -position '2:8' -filename test.ml < test.ml
  > EOF

  $ ocamlmerlin server -batch commands.txt | jq -c 
  {"class":"return","value":[],"notifications":[],"timing":{"clock":5,"cpu":4,"query":0,"pp":0,"reader":0,"ppx":0,"typer":3,"error":0},"heap_mbytes":1,"cache":{"reader_phase":"miss","ppx_phase":"miss","typer":"miss","cmt":{"hit":0,"miss":0},"cmi":{"hit":0,"miss":1}},"query_num":0}
  {"class":"return","value":[{"start":{"line":2,"col":8},"end":{"line":2,"col":9},"type":"int","tail":"no"},{"start":{"line":2,"col":8},"end":{"line":2,"col":9},"type":1,"tail":"no"},{"start":{"line":2,"col":8},"end":{"line":2,"col":13},"type":2,"tail":"no"}],"notifications":[],"timing":{"clock":0,"cpu":0,"query":0,"pp":0,"reader":0,"ppx":0,"typer":0,"error":0},"heap_mbytes":1,"cache":{"reader_phase":"miss","ppx_phase":"miss","typer":{"reused":2,"typed":0},"cmt":{"hit":0,"miss":0},"cmi":{"hit":1,"miss":0}},"query_num":1}
  {"class":"return","value":{"file":"$TESTCASE_ROOT/test.ml","pos":{"line":1,"col":4}},"notifications":[],"timing":{"clock":0,"cpu":0,"query":0,"pp":0,"reader":0,"ppx":0,"typer":0,"error":0},"heap_mbytes":1,"cache":{"reader_phase":"miss","ppx_phase":"miss","typer":{"reused":2,"typed":0},"cmt":{"hit":0,"miss":0},"cmi":{"hit":1,"miss":0}},"query_num":2}


query number should restart the count if the batch mode stopped the server
  $ ocamlmerlin server errors -filename test.ml < test.ml | jq -c 
  {"class":"return","value":[],"notifications":[],"timing":{"clock":3,"cpu":2,"query":0,"pp":0,"reader":0,"ppx":0,"typer":2,"error":0},"heap_mbytes":1,"cache":{"reader_phase":"miss","ppx_phase":"miss","typer":"miss","cmt":{"hit":0,"miss":0},"cmi":{"hit":0,"miss":1}},"query_num":0}

  $ $MERLIN server stop-server
