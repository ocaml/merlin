Companion to tests/test-dirs/locate/issue1748.t, for the `document` command.

The unit `Import` (here `import.ml = include Bar`) contains a submodule `Import`
(`Bar.Import`). In a `.mli`, `open! Import` used to resolve the bare `Import` in
the post-open environment, where the submodule shadows the unit, so `document`
returned the submodule's doc. 

  $ cat >dune-project <<'EOF'
  > (lang dune 3.0)
  > EOF

  $ cat >dune <<'EOF'
  > (library
  >  (name jump_to_wrong_import))
  > EOF

  $ cat >bar.ml <<'EOF'
  > (** SHADOW submodule doc *)
  > module Import = struct
  >   type hello = string
  > end
  > EOF

  $ cat >import.ml <<'EOF'
  > (** UNIT module doc *)
  > 
  > include Bar
  > EOF

  $ cat >foo.ml <<'EOF'
  > open! Import
  > EOF

  $ cat >foo.mli <<'EOF'
  > open! Import
  > EOF

  $ dune build @check 2>&1 | head -n 5

`document` on `Import` from `foo.mli` must return the unit's doc, not the
shadowing submodule's (before the fix this returned "SHADOW submodule doc"):

  $ $MERLIN single document -position 1:8 -look-for implementation \
  > -filename foo.mli <foo.mli
  {
    "class": "return",
    "value": "UNIT module doc",
    "notifications": []
  }

`document` from `foo.ml` behaves the same (it always did, the `.ml` leaf node
already carried the pre-open env):

  $ $MERLIN single document -position 1:8 -look-for implementation \
  > -filename foo.ml <foo.ml
  {
    "class": "return",
    "value": "UNIT module doc",
    "notifications": []
  }
