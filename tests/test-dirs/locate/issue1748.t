Reproduction for https://github.com/ocaml/merlin/issues/1748

When a module `Import` exists (here `import.ml`) and it includes another module
that itself contains a nested module named `Import` (here `Bar.Import`), calling
locate on `Import` from an `open Import` should jump to `import.ml` and not to 
`Bar.Import`.

  $ cat >dune-project <<'EOF'
  > (lang dune 3.0)
  > EOF

  $ cat >dune <<'EOF'
  > (library
  >  (name jump_to_wrong_import))
  > EOF

  $ cat >bar.ml <<'EOF'
  > module Import = struct
  >   type hello = string
  > end
  > EOF

  $ cat >import.ml <<'EOF'
  > include Bar
  > EOF

  $ cat >foo.ml <<'EOF'
  > open! Import
  > EOF

  $ cat >foo.mli <<'EOF'
  > open! Import
  > EOF

  $ dune build @check 2>&1 | head -n 5

Locating `Import` from `foo.ml` correctly jumps to `import.ml`:

  $ $MERLIN single locate -position 1:10 -look-for implementation \
  > -filename foo.ml <foo.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/import.ml",
      "pos": {
        "line": 1,
        "col": 0
      }
    },
    "notifications": []
  }

Locating `Import` from `foo.mli` should also jump to `import.ml`:

  $ $MERLIN single locate -position 1:10 -look-for implementation \
  > -filename foo.mli <foo.mli
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/import.ml",
      "pos": {
        "line": 1,
        "col": 0
      }
    },
    "notifications": []
  }
