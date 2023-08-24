Let's make sure that this test doesn't depend on previous state
  $ $MERLIN server stop-server

Let's create an environment with a simple PPX and a simple executable

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

  $ touch ppx_dep.txt

  $ cat > main.ml <<EOF
  > let () = print_int [%just_zero]
  > EOF

By default, the cache for the reader phase and the PPX phase are disabled
  $ cat > .merlin <<EOF
  > FLG -ppx '_build/default/.ppx/a56cb746ce56d7c281c7d9796f4166ed/ppx.exe -as-ppx
  > EOF

  $ dune exec ./main.exe 2>/dev/null
  0
  $ $MERLIN server errors -filename main.ml -log-file merlin_logs 1> /dev/null < main.ml
  $ cat merlin_logs | grep 'Phase cache' -A 1 | sed "s/[0-9]*//g"
  # . Phase cache - Reader phase
  Cache is disabled: configuration
  --
  # . Phase cache - PPX phase
  Cache is disabled: reader cache is disabled

The cache can be enabled via the USE_PPX_CACHE directive
  $ cat > .merlin <<EOF
  > FLG -ppx '_build/default/.ppx/a56cb746ce56d7c281c7d9796f4166ed/ppx.exe -as-ppx
  > USE_PPX_CACHE
  > EOF

  $ dune exec ./main.exe 2>/dev/null
  0
  $ $MERLIN server errors -filename main.ml -log-file merlin_logs 1> /dev/null < main.ml
  $ cat merlin_logs | grep 'Phase cache' -A 1 | sed "s/[0-9]*//g"
  # . Phase cache - Reader phase
  Cache wasn't populated
  --
  # . Phase cache - PPX phase
  Cache wasn't populated

  $ dune exec ./main.exe 2>/dev/null
  0
  $ $MERLIN server errors -filename main.ml -log-file merlin_logs 1> /dev/null < main.ml
  $ cat merlin_logs | grep 'Phase cache' -A 1 | sed "s/[0-9]*//g"
  # . Phase cache - Reader phase
  Cache hit
  # . Phase cache - PPX phase
  Cache hit


Modifying the source code invalidates the cache
  $ cat >main.ml <<EOF
  > let _ = print_int [%just_zero]
  > EOF

  $ dune exec ./main.exe 2>/dev/null
  0
  $ $MERLIN server errors -filename main.ml -log-file merlin_logs 1> /dev/null < main.ml
  $ cat merlin_logs | grep 'Phase cache' -A 1 | sed "s/[0-9]*//g"
  # . Phase cache - Reader phase
  Cache invalidation
  --
  # . Phase cache - PPX phase
  Cache invalidation

Also, modifying the PPX invalidates the PPX cache

(Same PPX as before)
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

  $ dune exec ./main.exe 2>/dev/null
  0
  $ $MERLIN server errors -filename main.ml -log-file merlin_logs 1> /dev/null < main.ml
  $ cat merlin_logs | grep 'Phase cache' -A 1 | sed "s/[0-9]*//g"
  # . Phase cache - Reader phase
  Cache hit
  # . Phase cache - PPX phase
  Cache hit

(Different PPX than before: expands to 1, instead of 0)
  $ cat > my_ppx.ml <<EOF
  > open Ppxlib
  > let rule =
  >    let ppx =
  >      let ast_context = Extension.Context.expression in
  >      let pl = Ast_pattern.(pstr nil) in
  >      let expand_fn ~ctxt =
  >        let loc = Expansion_context.Extension.extension_point_loc ctxt in
  >        Ast_builder.Default.eint ~loc 1 in
  >      Extension.V3.declare "just_zero" ast_context pl expand_fn
  > in
  > Ppxlib.Context_free.Rule.extension ppx
  > let () = Driver.register_transformation ~rules:[ rule ] "just_zero"
  > EOF

  $ dune exec ./main.exe 2>/dev/null
  1
  $ $MERLIN server errors -filename main.ml -log-file merlin_logs 1> /dev/null < main.ml
  $ cat merlin_logs | grep 'Phase cache' -A 1 | sed "s/[0-9]*//g"
  # . Phase cache - Reader phase
  Cache hit
  # . Phase cache - PPX phase
  Cache invalidation

Also, modifying the args to the PPX invalidates the PPX cache
  $ cat > .merlin <<EOF
  > FLG -ppx '_build/default/.ppx/a56cb746ce56d7c281c7d9796f4166ed/ppx.exe -as-ppx -no-color
  > USE_PPX_CACHE
  > EOF

  $ dune exec ./main.exe 2>/dev/null
  1
  $ $MERLIN server errors -filename main.ml -log-file merlin_logs 1> /dev/null < main.ml
  $ cat merlin_logs | grep 'Phase cache' -A 1 | sed "s/[0-9]*//g"
  # . Phase cache - Reader phase
  Cache hit
  # . Phase cache - PPX phase
  Cache invalidation

-------------

However, modifying PPX dependencies doesn't invalidate the PPX cache
and therefore can lead to wrong Merlin output, if the cache is enabled.

Let's create a PPX with PPX dependency
  $ cat > my_ppx.ml <<EOF
  > open Ppxlib
  > let rule =
  >    let ppx =
  >      let ast_context = Extension.Context.expression in
  >      let pl = Ast_pattern.(pstr nil) in
  >      let expand_fn ~ctxt =
  >        let c = open_in_bin "ppx_dep.txt" in
  >        let s = input_line c in
  >        let () = close_in c in
  >        let loc = Expansion_context.Extension.extension_point_loc ctxt in
  >        match int_of_string_opt s with
  >        | None -> Ast_builder.Default.pexp_extension ~loc @@ Location.error_extensionf ~loc "It's sunny"
  >        | Some i -> Ast_builder.Default.eint ~loc i
  >      in
  >      Extension.V3.declare "just_zero" ast_context pl expand_fn
  > in
  > Ppxlib.Context_free.Rule.extension ppx
  > let () = Driver.register_transformation ~rules:[ rule ] "just_zero"
  > EOF

When ppx_dep.txt contains an int, there are no errors.
  $ cat > ppx_dep.txt <<EOF
  > 1
  > EOF

  $ dune exec ./main.exe 2>/dev/null
  1
And Merlin does the right thing.
  $ $MERLIN server errors -filename main.ml -log-file merlin_logs 1> /dev/null < main.ml
  $ $MERLIN server errors -filename main.ml -log-file merlin_logs < main.ml
  {
    "class": "return",
    "value": [],
    "notifications": []
  }
  $ cat merlin_logs | grep 'Phase cache' -A 1 | sed "s/[0-9]*//g"
  # . Phase cache - Reader phase
  Cache hit
  # . Phase cache - PPX phase
  Cache hit

When ppx_dep.txt is changed to contain a non-int, the AST contains an error.
  $ cat > ppx_dep.txt <<EOF
  > this isn't an int
  > EOF

  $ dune exec ./main.exe 2>/dev/null
  [1]

Merlin just uses the cache, though, and doesn't notice the ppx_dep.txt change.
  $ $MERLIN server errors -filename main.ml -log-file merlin_logs < main.ml
  {
    "class": "return",
    "value": [],
    "notifications": []
  }
  $ cat merlin_logs | grep 'Phase cache' -A 1 | sed "s/[0-9]*//g"
  # . Phase cache - Reader phase
  Cache hit
  # . Phase cache - PPX phase
  Cache hit

That's why it's important that the build system doesn't enable the cache
when the project has PPX dependencies
  $ cat > .merlin <<EOF
  > FLG -ppx '_build/default/.ppx/a56cb746ce56d7c281c7d9796f4166ed/ppx.exe -as-ppx
  > EOF

Again: when ppx_dep.txt contains an int, there are no errors.
  $ cat > ppx_dep.txt <<EOF
  > 1
  > EOF

  $ dune exec ./main.exe 2>/dev/null
  1

And Merlin does the right thing.
  $ $MERLIN server errors -filename main.ml -log-file merlin_logs < main.ml
  {
    "class": "return",
    "value": [],
    "notifications": []
  }
  $ cat merlin_logs | grep 'Phase cache' -A 1 | sed "s/[0-9]*//g"
  # . Phase cache - Reader phase
  Cache is disabled: configuration
  --
  # . Phase cache - PPX phase
  Cache is disabled: reader cache is disabled

Again: when ppx_dep.txt is changed to contain a non-int, the AST contains an error.
  $ cat > ppx_dep.txt <<EOF
  > this isn't an int
  > EOF

  $ dune exec ./main.exe 2>/dev/null
  [1]

This time, since the PPX cache isn't enabled, Merlin does the right thing here as well.
  $ $MERLIN server errors -filename main.ml -log-file merlin_logs < main.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 18
        },
        "end": {
          "line": 1,
          "col": 30
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "It's sunny"
      }
    ],
    "notifications": []
  }
  $ cat merlin_logs | grep 'Phase cache' -A 1 | sed "s/[0-9]*//g"
  # . Phase cache - Reader phase
  Cache is disabled: configuration
  --
  # . Phase cache - PPX phase
  Cache is disabled: reader cache is disabled

Let's clean up
  $ $MERLIN server stop-server
