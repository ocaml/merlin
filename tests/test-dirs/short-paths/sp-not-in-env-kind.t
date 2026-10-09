A candidate that is not valid in one printing environment is put aside, and
restored when the environment changes. It should be correctly restored.

  $ cat >test.ml <<'EOF'
  > module M = struct module type S = sig end end
  > let _ = let open M in (1 : (module M.S))
  > let _ : (module M.S) = 1
  > EOF

FIXME: The first error, printed in second, should show [module S]
  $ $MERLIN single errors -short-paths -filename test.ml < test.ml \
  > | tr '\n' ' ' | jq -r '.value[] | "\(.start.line): \(.message)"'
  2: The constant 1 has type int but an expression was expected of type   (module M.S)
  3: The constant 1 has type int but an expression was expected of type   (module M.S)
