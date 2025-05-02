  $ cat >dune-project <<'EOF'
  > (lang dune 3.0)
  > EOF

  $ mkdir dep
  $ cat >dep/dep.ml <<'EOF'
  > let txt = "Hello!"
  > EOF

  $ mkdir exe
  $ cat >exe/main.ml << 'EOF'
  > print_endline Dep.txt
  > EOF

  $ cat >exe/dune << 'EOF'
  > (executable
  >  (name main))
  > (copy_files# %{project_root}/dep/*.ml)
  > EOF

  $ dune exec ./exe/main.exe
  Hello!

  $ $MERLIN single errors -filename exe/main.ml <exe/main.ml | jq '.value'
  []

FIXME: Dune should also provide a configuration for the original sources
  $ $MERLIN single errors -filename dep/dep.ml <dep/dep.ml | jq '.value'
  [
    {
      "type": "config",
      "sub": [],
      "valid": true,
      "message": "No config found for file dep/dep.ml. Try calling 'dune build'."
    }
  ]
