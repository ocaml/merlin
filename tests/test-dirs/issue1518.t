  $ cat >dune-project <<EOF
  > (lang dune 2.0)
  > EOF

  $ cat >main.ml <<EOF
  > print_endline "42"
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (flags :standard -safe-string))
  > EOF


  $ dune exec ./main.exe
  42

FIXME: in 5.0 the compiler still accept the deleted flag "-safe-string". It
simply is a noop. Merlin should ignore it as well.
  $ $MERLIN single errors -filename main.ml <main.ml |
  > jq '.value'
  [
    {
      "type": "config",
      "sub": [],
      "valid": true,
      "message": "unknown flag -safe-string"
    }
  ]
