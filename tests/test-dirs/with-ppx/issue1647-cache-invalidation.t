Make sure that this test doesn't depend on previous state
  $ $MERLIN server stop-server

  $ cat >dune-project <<EOF
  > (lang dune 2.0)
  > EOF

  $ cat > my_ppx.ml <<EOF
  > open Ppxlib
  > let rule =
  >    let ppx =
  >      let ast_context = Extension.Context.expression in
  >      let pl = Ast_pattern.(pstr nil) in
  >      let expand_fn ~ctxt =
  >        let loc = Expansion_context.Extension.extension_point_loc ctxt in
  >        Ast_builder.Default.eint ~loc 0 in
  >      Extension.V3.declare "just_zero" ast_context pl expand_fn
  > in
  > Ppxlib.Context_free.Rule.extension ppx
  > let () = Driver.register_transformation ~rules:[ rule ] "just_zero"
  > EOF

  $ cat > main.ml <<EOF
  > let () = print_int [%just_zero]
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name my_ppx)
  >  (kind ppx_rewriter)
  >  (modules my_ppx)
  >  (libraries ppxlib))
  > 
  > (executable
  >  (name main)
  >  (modules main)
  >  (preprocess
  >   (pps my_ppx)))
  > EOF

  $ cat > .merlin <<EOF
  > FLG -ppx '_build/default/.ppx/a56cb746ce56d7c281c7d9796f4166ed/ppx.exe -as-ppx
  > USE_PPX_CACHE
  > EOF

  $ dune exec ./main.exe 2>/dev/null
  0

`errors` requires the typedtree and thus types the buffer
  $ $MERLIN server errors -log-file merlin_logs \
  > -filename main.ml 1> /dev/null <main.ml
  $ cat merlin_logs | grep 'Phase cache' -A 1 | sed "s/[0-9]*//g"
  # . Phase cache - Reader phase
  Cache wasn't populated
  --
  # . Phase cache - PPX phase
  Cache wasn't populated

Now we edit the file. This should invalidate the caches
  $ cat > main.ml <<EOF
  > let () = print_string [%just_zero]
  > EOF


The `dump source` command only requires the parsetree and only invalidates the
reader cache:
  $ $MERLIN server dump -what source  -log-file merlin_logs \
  > -filename main.ml <main.ml | tr -d '\n' | jq '.value'
  "let () = print_string ([%just_zero ])"

  $ cat merlin_logs | grep 'Phase cache' -A 1 | sed "s/[0-9]*//g"
  # . Phase cache - Reader phase
  Cache invalidation

The cache detects that the reader cache was invalidated in the last query
and therefore invalidates the PPX cache in this query.
  $ $MERLIN server dump -what ppxed-source  -log-file merlin_logs \
  > -filename main.ml <main.ml | tr -d '\n' | jq '.value'
  "let () = print_string 0"

  $ cat merlin_logs | grep 'Phase cache' -A 1 | sed "s/[0-9]*//g"
  # . Phase cache - Reader phase
  Cache hit
  # . Phase cache - PPX phase
  Cache invalidation

Stop server
  $ $MERLIN server stop-server
