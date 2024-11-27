  $ cat >dune-project <<'EOF'
  > (lang dune 3.0)
  > EOF

  $ cat >dune <<'EOF'
  > (executable (name foo))
  > (executable (name foobar) (libraries yojson))
  > EOF

  $ touch foo.ml 

  $ cat >foobar.ml <<'EOF'
  > let eq = Yojson.equal
  > let () = Printf.printf "%b" @@ eq `Null `Null
  > EOF

  $ $DUNE build 
  $ $DUNE exec ./foobar.exe
  true

FIXME: there appear to be an issue with configuration here
  $ $MERLIN single locate -position 1:10 \
  > -filename foobar.ml <foobar.ml 
  {
    "class": "return",
    "value": "Not in environment 'Yojson'",
    "notifications": []
  }
